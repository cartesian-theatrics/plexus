(ns plexus.surface
  "Immutable triangle-surface navigation. Distances are measured on the mesh,
  and headings are parallel transported by unfolding adjacent triangles."
  (:refer-clojure :exclude [abs])
  (:require [clj-manifold3d.core :as m]))

(defn finite? [x]
  (and (number? x) (#?(:clj Double/isFinite :cljs js/Number.isFinite) (double x))))
(defn v3? [v] (and (sequential? v) (= 3 (count v)) (every? finite? v)))
(defn add [a b] (mapv + a b))
(defn sub [a b] (mapv - a b))
(defn scale [a s] (mapv #(* % s) a))
(defn dot [a b] (reduce + (map * a b)))
(defn cross [[a b c] [x y z]]
  [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
(defn length [v] (#?(:clj Math/sqrt :cljs js/Math.sqrt) (dot v v)))
(defn abs [x] (#?(:clj Math/abs :cljs js/Math.abs) (double x)))
(defn unit [v]
  (let [n (length v)]
    (when-not (and (finite? n) (> n 1e-12))
      (throw (ex-info "Surface direction must be finite, nonzero, and tangent" {:vector v})))
    (scale v (/ n))))

(defrecord Surface [positions faces neighbors epsilon corner-normals])
(defn surface? [x] (instance? Surface x))

(defn- smooth-normals [positions faces neighbors]
  ;; Match native SurfaceUV: angle-weighted corner normals, connected through
  ;; physical halfedges, with creases over 60 degrees kept sharp.
  (let [parent (atom (vec (range (* 3 (count faces)))))
        root (fn [i] (loop [i i] (let [p (get @parent i)] (if (= p i) i (recur p)))))
        join! (fn [a b] (swap! parent assoc (root a) (root b)))]
    (doseq [f (range (count faces)) i (range 3)
            :let [[other j] (get-in neighbors [f i])]
            :when (and (< f other) (>= (dot (:normal (faces f)) (:normal (faces other))) 0.5))]
      (join! (+ (* 3 f) i) (+ (* 3 other) (mod (inc j) 3)))
      (join! (+ (* 3 f) (mod (inc i) 3)) (+ (* 3 other) j)))
    (let [sums (reduce (fn [sums [f i]]
                         (let [vs (:vertices (faces f)) p (positions (vs i))
                               a (unit (sub (positions (vs (mod (inc i) 3))) p))
                               b (unit (sub (positions (vs (mod (+ i 2) 3))) p))
                               angle (#?(:clj Math/atan2 :cljs js/Math.atan2) (length (cross a b)) (dot a b))]
                           (update sums (root (+ (* 3 f) i)) (fnil add [0 0 0])
                                   (scale (:normal (faces f)) angle))))
                       {} (for [f (range (count faces)) i (range 3)] [f i]))]
      (mapv (fn [f] (mapv #(unit (get sums (root (+ (* 3 f) %)))) (range 3)))
            (range (count faces))))))

(defn prepare
  "Copy a Manifold into a reusable, immutable surface. Property seams are joined
  using explicit merge IDs, never proximity. Refine smooth tangents beforehand."
  [manifold]
  (when-not (m/manifold? manifold)
    (throw (ex-info "surface expects a Manifold" {})))
  (let [mesh (m/get-mesh manifold)
        data (try (m/mesh-data mesh)
                  (finally #?(:clj (.close mesh) :cljs (m/dispose! mesh))))
        {:keys [num-prop vert-properties tri-verts merge-from-vert merge-to-vert halfedge-tangent]} data
        positions (mapv #(vec (take 3 %)) (partition num-prop vert-properties))
        parent (atom (vec (range (count positions))))
        root (fn [v] (loop [v v] (let [p (get @parent v)] (if (= p v) v (recur p)))))]
    ;; Boolean composition may emit zero-filled tangents for flat faces.
    (when (or (empty? tri-verts) (some #(not (zero? %)) halfedge-tangent))
      (throw (ex-info "Surface requires a nonempty triangle mesh; refine smooth tangents first" {})))
    (doseq [[a b] (map vector merge-from-vert merge-to-vert)]
      (swap! parent assoc (root a) (root b)))
    (let [faces (mapv (fn [indices]
                        (let [ids (mapv root indices)
                              [a b c] (mapv positions ids)
                              normal (unit (cross (sub b a) (sub c a)))]
                          {:vertices ids :normal normal})) tri-verts)
          edges (reduce-kv (fn [edges f {:keys [vertices]}]
                             (reduce (fn [edges i]
                                       (let [edge [(vertices i) (vertices (mod (inc i) 3))]]
                                         (when (contains? edges edge)
                                           (throw (ex-info "Surface has an unpaired or nonmanifold edge" {:edge edge})))
                                         (assoc edges edge [f i]))) edges (range 3))) {} faces)
          neighbors (mapv (fn [{:keys [vertices]}]
                            (mapv (fn [i]
                                    (or (get edges [(vertices (mod (inc i) 3)) (vertices i)])
                                        (throw (ex-info "Surface requires closed, paired halfedges" {}))))
                                  (range 3))) faces)
          lo (apply mapv min positions)
          hi (apply mapv max positions)]
      (->Surface positions faces neighbors (max 1e-9 (* 1e-9 (length (sub hi lo))))
                 (smooth-normals positions faces neighbors)))))

(defn normal [{:keys [surface face]}] (get-in surface [:faces face :normal]))
(defn bary [{:keys [positions faces]} face p]
  (let [[a b c] (mapv positions (:vertices (faces face)))
        ab (sub b a) ac (sub c a) n (cross ab ac)
        denominator (dot n n)
        v (/ (dot (cross (sub p a) ac) n) denominator)
        w (/ (dot (cross ab (sub p a)) n) denominator)]
    [(- 1 v w) v w]))

(defn- bary-tolerance [{:keys [positions faces epsilon]} face]
  (let [[a b c] (mapv positions (:vertices (faces face)))]
    (/ (* epsilon (max (length (sub a b)) (length (sub b c)) (length (sub c a))))
       (length (cross (sub b a) (sub c a))))))

(defn- closest [surface p]
  (let [{:keys [positions faces]} surface]
    (reduce (fn [best face]
              (let [{:keys [vertices normal]} (faces face)
                    a (positions (first vertices))
                    q (sub p (scale normal (dot (sub p a) normal)))
                    candidates (if (every? #(>= % 0) (bary surface face q)) [q]
                                   (for [i (range 3)
                                         :let [a (positions (vertices i))
                                               d (sub (positions (vertices (mod (inc i) 3))) a)]]
                                     (add a (scale d (max 0 (min 1 (/ (dot (sub p a) d) (dot d d))))))))]
                (reduce (fn [best q]
                          (let [distance (dot (sub q p) (sub q p))]
                            (if (< distance (:distance best))
                              {:face face :point q :distance distance} best))) best candidates)))
            {:distance ##Inf} (range (count faces)))))

(defn attach [surface {:keys [origin direction depth step-size]
                      :or {depth 0 step-size 1}}]
  (when-not (and (v3? origin) (v3? direction) (finite? depth)
                 (finite? step-size) (pos? step-size))
    (throw (ex-info "Surface requires :origin, :direction, finite :depth, and positive :step-size" {})))
  (let [cursor (assoc (dissoc (closest surface origin) :distance)
                      :surface surface :depth depth :step-size step-size)
        point (:point cursor)
        desired (unit direction)
        ;; At a seed edge/vertex, the supplied direction selects an outgoing face.
        candidates (for [face (range (count (:faces surface)))
                         :let [c (assoc cursor :face face)
                               n (normal c)
                               a (get-in surface [:positions (get-in surface [:faces face :vertices 0])])
                               b (bary surface face point)
                               tolerance (* 4 (bary-tolerance surface face))
                               tangent (sub desired (scale n (dot desired n)))]
                         :when (and (<= (abs (dot (sub point a) n)) (* 4 (:epsilon surface)))
                                    (every? #(>= % (- tolerance)) b)
                                    (> (length tangent) 1e-12))
                         :let [forward (unit tangent)
                               db (sub (bary surface face (add point forward)) b)]
                         :when (every? (fn [[b db]] (or (> b tolerance) (>= db -1e-10))) (map vector b db))]
                     (assoc c :forward forward :seed? true :alignment (dot desired forward)))]
    (if (seq candidates)
      (dissoc (apply max-key :alignment candidates) :alignment)
      (throw (ex-info "Surface direction must have an outgoing tangent at the origin" {:point point})))))

(defn- transport [v a b]
  ;; Rodrigues' minimal rotation maps the old face normal to the new one.
  (let [axis (cross a b) cosine (max -1 (min 1 (dot a b)))]
    (when (< cosine -0.999999999)
      (throw (ex-info "Surface walk encountered a folded-back edge" {})))
    (add v (add (cross axis v) (scale (cross axis (cross axis v)) (/ (+ 1 cosine)))))))

(defn- enter
  "Find the face a ray enters, unfolding its heading through the incident fan.
  Curvature concentrated at a vertex makes continuation ambiguous: reject it."
  [cursor direction]
  (let [{:keys [surface point face]} cursor
        {:keys [faces neighbors epsilon]} surface
        b (bary surface face point)
        tolerance (* 4 (bary-tolerance surface face))
        vertex? (>= (count (filter #(<= (abs %) tolerance) b)) 2)
        derivative (sub (bary surface face (add point direction)) b)
        seed-enters? (and (:seed? cursor)
                          (every? (fn [[b db]] (or (> b tolerance) (>= db -1e-10)))
                                  (map vector b derivative)))]
    (when (and vertex? (not seed-enters?))
      (let [vertex (get-in faces [face :vertices (first (apply max-key second (map-indexed vector b)))])
            angles (for [{:keys [vertices]} faces
                         :let [i (first (keep-indexed #(when (= %2 vertex) %1) vertices))]
                         :when (some? i)
                         :let [p ((:positions surface) vertex)
                               a (unit (sub ((:positions surface) (vertices (mod (inc i) 3))) p))
                               b (unit (sub ((:positions surface) (vertices (mod (+ i 2) 3))) p))]]
                     (#?(:clj Math/atan2 :cljs js/Math.atan2) (length (cross a b)) (dot a b)))]
        (when (> (abs (- (reduce + angles) (* 2 #?(:clj Math/PI :cljs js/Math.PI)))) 1e-6)
          (throw (ex-info "Surface walk hits a vertex with ambiguous continuation; change the origin or heading"
                          {:reason :vertex-singularity :face face :point point})))))
    (loop [pending [[cursor direction]] seen #{} index 0]
      (when (= index (count pending))
        (throw (ex-info "Surface walk could not enter an adjacent triangle" {:face face :point point})))
      (let [[c d] (pending index) f (:face c)]
        (if (seen f)
          (recur pending seen (inc index))
          (let [b (bary surface f point)
                tolerance (* 4 (bary-tolerance surface f))
                derivative (sub (bary surface f (add point d)) b)
                exits (keep (fn [[b db]] (when (< db -1e-12) (/ (- (max 0 b)) db)))
                            (map vector b derivative))
                distance (if (seq exits) (apply min exits) ##Inf)
                enters? (and (every? #(>= % (- tolerance)) b)
                             (every? (fn [[b db]] (or (> b tolerance) (>= db -1e-10))) (map vector b derivative))
                             (> distance epsilon) (finite? distance))]
            (if enters?
              [c d distance]
              (let [next-faces (for [i (range 3)
                                     :when (<= (abs (b (mod (+ i 2) 3))) tolerance)
                                     :let [[other _] (get-in neighbors [f i])
                                           a (normal c) n (get-in faces [other :normal])]]
                                 [(assoc c :face other :forward (unit (transport (:forward c) a n)))
                                  (unit (transport d a n))])]
                (recur (into pending next-faces) (conj seen f) (inc index))))))))))

(defn walk
  "Walk signed local X/Z distances as one tangent vector. Return every crossed
  edge plus the endpoint; no closest-point reprojection is used after attachment."
  [cursor x z]
  (let [n (normal cursor) right (cross n (:forward cursor))
        displacement (add (scale right x) (scale (:forward cursor) z))
        distance (length displacement)
        epsilon (get-in cursor [:surface :epsilon])]
    (if (<= distance epsilon) [cursor]
        (loop [cursor cursor direction (unit displacement) remaining distance
               samples [cursor] crossings 0]
          (when (> crossings 100000)
            (throw (ex-info "Surface walk exceeded 100000 triangle crossings; split the movement" {})))
          (if (<= remaining epsilon) samples
              (let [[entered direction exit] (enter cursor direction)
                    step (min remaining exit)
                    end (-> entered (dissoc :seed?) (update :point add (scale direction step)))]
                (recur end direction (- remaining step) (conj samples end) (inc crossings))))))))

(defn move [cursor x y z]
  (when-not (every? finite? [x y z])
    (throw (ex-info "Surface movement must be finite" {})))
  (update (peek (walk cursor x z)) :depth + y))

(defn turn [cursor angle]
  (when-not (finite? angle) (throw (ex-info "Surface heading angle must be finite" {})))
  (let [f (:forward cursor) r (cross (normal cursor) f)]
    (assoc cursor :forward (unit (add (scale f (#?(:clj Math/cos :cljs js/Math.cos) angle))
                                      (scale r (#?(:clj Math/sin :cljs js/Math.sin) angle)))))))

(defn frame-normal
  "Continuous interpolated normal within a smooth fan; creases remain sharp.
  Navigation itself still uses the exact triangle normals."
  [{:keys [surface face point]}]
  (unit (reduce add (map scale (get-in surface [:corner-normals face])
                         (bary surface face point)))))

(defn frame-columns [cursor axis]
  (let [y (frame-normal cursor)
        z (unit (sub (:forward cursor) (scale y (dot (:forward cursor) y))))
        x (cross y z)
        p (add (:point cursor) (scale y (:depth cursor)))]
     (case axis
       :z [x y z p]
       :x [(scale z -1) y x p]
       :y [x (scale z -1) y p])))

(defn frame
  ([cursor] (frame cursor :z))
  ([cursor axis] (apply m/frame (frame-columns cursor axis))))

(defn sweep-samples
  "Coalesce nearby stations that invert a profile within a smooth fan. Reject
  folds across sharp creases, endpoint collapse, or coalescing beyond twice the
  requested spacing. Navigation samples are left unchanged. This is not a test
  for collisions between distant portions of a path."
  [samples axis distance profile n-steps]
  (let [sign (if (neg? distance) -1 1)
        epsilon (get-in (first samples) [:surface :epsilon])
        columns (mapv #(frame-columns % axis) samples)
        sharp (vec (reductions + 0 (map (fn [a b] (if (< (dot (normal a) (normal b)) 0.5) 1 0))
                                       samples (next samples))))
        stations (vec (reductions + 0 (map (fn [a b]
                                             (if (= axis :y) (abs (- (:depth b) (:depth a)))
                                                 (length (sub (:point b) (:point a)))))
                                           samples (next samples))))
        limit (* 2 (if n-steps (/ (abs distance) n-steps) (:step-size (first samples))))
        fold (fn [i j]
               (let [[x0 y0 z0 p0] (columns i) [x1 y1 z1 p1] (columns j)]
                 (first (filter
                         (fn [[u v]]
                           (let [a (add p0 (add (scale x0 u) (scale y0 v)))
                                 b (add p1 (add (scale x1 u) (scale y1 v)))
                                 delta (sub b a)]
                             (<= (min (* sign (dot delta z0)) (* sign (dot delta z1))) epsilon)))
                         profile))))
        fail! (fn [i j vertex]
                (throw (ex-info "Surface sweep folds or collapses at the profile; reduce the profile/depth or smooth the support"
                                {:reason :surface-sweep-fold :sample-index i :profile-point vertex
                                 :point (:point (samples i)) :face (:face (samples i))
                                 :next-face (:face (samples j)) :axis axis})))]
    (loop [kept [0] j 1]
      (if (= j (count samples)) (mapv samples kept)
          (let [i (peek kept)
                vertex (fold i j)]
            (when (> (- (stations j) (stations i)) (+ limit epsilon))
              (fail! i j vertex))
            (if-not vertex
              (recur (conj kept j) (inc j))
              (cond
                (> (sharp j) (sharp i)) (fail! i j vertex)
                (< j (dec (count samples))) (recur kept (inc j))
                (> (count kept) 1) (recur (pop kept) j)
                :else (fail! i j vertex))))))))

(defn sample [cursor axis distance n-steps]
  (when-not (finite? distance) (throw (ex-info "Surface distance must be finite" {})))
  (let [steps (or n-steps (max 1 (long (#?(:clj Math/ceil :cljs js/Math.ceil)
                                      (/ (abs distance) (:step-size cursor))))))]
    (when (> steps 100000)
      (throw (ex-info "Surface movement exceeds 100000 samples; increase :step-size" {})))
    (loop [i 0 cursor cursor samples [cursor]]
      (when (> (count samples) 100000)
        (throw (ex-info "Surface movement exceeds 100000 samples; split the movement" {})))
      (if (= i steps) samples
          (let [d (/ distance steps)
                path (if (= axis :y) [cursor (update cursor :depth + d)]
                         (walk cursor (if (= axis :x) d 0) (if (= axis :z) d 0)))]
            (recur (inc i) (peek path) (into samples (next path))))))))
