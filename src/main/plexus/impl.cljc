(ns plexus.impl
  (:require
   #?(:clj [clj-manifold3d.impl :as impl])
   [malli.core :as ma]
   [clj-manifold3d.core :as m]
   [plexus.utils :as u]
   [plexus.surface :as surface]
   [plexus.transforms :as tf]))

(declare extrude*)

(defrecord Extrusion [result-forms frames state angle-scalar forms transforms models main-model]
  Object
  (toString [_]
    (str "Extrusion, main model: " main-model))

  #?@(:clj [impl/ICSGConvertable
            (to-csg [this] (get models main-model))]))

#?(:clj
   (defmethod print-method Extrusion [x writer]
     (.write writer (str "Extrusion:" (:main-model x)))))

(defrecord Frame [frame-transform segment-transform segments])

(defrecord Segment [start-transform end-transform all-transforms cross-section manifold is-transformed])

(defrecord Model [frame-transform manifold is-transformed])

(defn frame? [x]
  (instance? Frame x))

(defn model? [x]
  (instance? Model x))

(defn extrusion?
  "True if `x` is an extrusion."
  [x]
  (instance? Extrusion x))

(defn form?
  [x]
  (and (map? x) (keyword? (:op x))))

(defn to-model [frame]
  (let [manifolds (remove nil? (map :manifold (:segments frame)))
        manifold (if (pos? (count manifolds))
                   (m/union manifolds)
                   (m/manifold))]
    (Model. (:frame-transform frame) manifold false)))

(defn project-model [model]
  (if (:is-transformed model)
    model
    (let [tf (:frame-transform model)
          manifold (:manifold model)]
      (Model. tf (m/transform manifold tf) true))))

(defn unproject-model [model]
  (if (:is-transformed model)
    (let [tf (:frame-transform model)
          manifold (:manifold model)]
      (Model. tf (m/transform manifold (m/invert-frame tf)) true))
    model))

(defn to-manifold [x]
  (cond (frame? x) (-> x to-model project-model :manifold)
        (model? x) (-> x project-model :manifold)
        (m/manifold? x) x
        (extrusion? x) (get (:models x) (:main-model x))
        :else (throw (ex-info "Argument must be Frame, Model or Manifold" {:value x}))))

(def registry
  (merge
   (ma/class-schemas)
   (ma/comparator-schemas)
   (ma/base-schemas)
   (ma/predicate-schemas)
   {:neg-int (ma/-simple-schema {:type :neg-int, :pred neg-int?})
    :pos-int (ma/-simple-schema {:type :pos-int, :pred pos-int?})
    :extrusion (ma/-simple-schema {:type :extrusion :pred extrusion?})
    :manifold (ma/-simple-schema {:type :manifold :pred m/manifold?})
    :cross-section (ma/-simple-schema {:type :cross-section :pred m/cross-section?})
    :surface (ma/-simple-schema {:type :surface :pred #(or (surface/surface? %) (m/manifold? %))})
    :transform (ma/-simple-schema {:type :transform :pred tf/transform?})}))

(def get-schema-validator
  (memoize
   (fn [schema]
     (ma/validator schema {:registry registry}))))

(defn check-schema [x schema form]
  (let [validator (get-schema-validator schema)]
    (if (validator x) x (throw (ex-info "Schema validation failure"
                                        (assoc (ma/explain schema x {:registry registry}) :form form :op (first form)))))))

(defn parse-path
  "Split leading keyword/value options (or an options map) from a body."
  [path-spec]
  (if (and (map? (first path-spec)) (not (form? (first path-spec))))
    [(first path-spec) (vec (next path-spec))]
    (loop [remaining (seq path-spec) args {}]
      (if (keyword? (first remaining))
        (do
          (when-not (next remaining)
            (throw (ex-info "Missing option value" {:option (first remaining)})))
          (recur (nnext remaining) (assoc args (first remaining) (second remaining))))
        [args (vec remaining)]))))

(defn parse-container [args]
  (if (and (map? (first args)) (not (form? (first args))))
    [(first args) (vec (next args))]
    (loop [args (seq args) opts {}]
      (if (#{:to :algorithm} (first args))
        (do
          (when-not (next args)
            (throw (ex-info "Missing option value" {:option (first args)})))
          (recur (nnext args) (assoc opts (first args) (second args))))
        [opts (vec args)]))))

(defn parse-args
  ([form] (parse-args form nil))
  ([form schema]
   (let [op (first form)]
     (if (#{::union ::difference ::intersection} op)
       {:op op ::list (next form)}
       (let [[opts body] ((if (#{::hull ::loft} op) parse-container parse-path) (next form))
             parsed (cond-> (assoc opts :op op) (seq body) (assoc ::list body))]
         (if schema (check-schema parsed schema form) parsed))))))

(defn normalize-segment [segment]
  (cond
    (nil? segment) []
    (sequential? segment)
    (into []
          (comp (mapcat normalize-segment)
                (remove nil?))
          segment)
    :else [segment]))

(defn ->keyword [namespace name*]
  (keyword (name namespace) (name name*)))

(defn all-transforms [start-transform tf-fn curve-radius angle steps step-fn]
  (let [step-angle (/ angle steps)]
    (map-indexed (fn [i tf]
                   (step-fn tf i))
                 (take (inc steps)
                       (iterate
                        (fn [transform]
                          (let [d (u/bAc->a curve-radius step-angle curve-radius)
                                r (- (/ u/pi 2) (/ (- u/pi step-angle) 2))]
                            (tf-fn transform r d step-angle)))
                        start-transform)))))

(def source-keys [:file :line :column :source-form])

(defn attach-source [source form]
  (cond
    (sequential? form) (mapv #(attach-source source %) form)
    (or (map? form) (fn? form)) (with-meta form (merge source (meta form)))
    :else form))

(defn error-with-context
  "Keep the innermost operation, its arguments, source location, and cause."
  [error form source]
  (let [data (ex-data error)
        op (or (:key data) (:op data) (:op form) (:op source))
        source (merge (select-keys source source-keys)
                      (select-keys (meta form) source-keys)
                      (select-keys data source-keys))
        original (or (:original-exception data) error)
        message (str (if op (str "Error while applying `" op "`") "Error while building Plexus form")
                     (when (seq source)
                       (str " at " (:file source) ":" (:line source) ":" (:column source)))
                     ": " #?(:clj (.getMessage original) :cljs (.-message original)))]
    (ex-info message
             (merge data source
                    {:message message :key op :op op
                     :form (or (:form data) form)
                     :original-exception original})
             error)))

(defn resolve-form [form]
  (if (fn? form)
    (recur (attach-source (meta form) (form)))
    form))

(defn- surface-frame [frame cursor]
  (assoc frame :surface-cursor cursor
         :segment-transform (m/compose-frames (m/invert-frame (:frame-transform frame))
                                               (surface/frame cursor))))

(defn- surface-forward [frame form frame-id axis distance]
  (when-let [option (first (filter #(some? (get form %)) [:model :transform-step-fn :twist]))]
    (throw (ex-info "Surface forward does not support this option" {:option option})))
  (let [{:keys [center branch? n-steps gap props]} form
        original (:surface-cursor frame)
        start (if center (peek (surface/sample original axis (- (/ distance 2)) n-steps)) original)
        samples (surface/sample start axis distance n-steps)
        inverse (m/invert-frame (:frame-transform frame))
        transforms (mapv #(m/compose-frames inverse (surface/frame % axis)) samples)
        end (surface-frame frame (peek samples))
        cross-section (when-not (or (true? gap)
                                     (and (sequential? gap) (contains? (set gap) frame-id)))
                        (:cross-section frame))
        manifold (when (and cross-section (> (count transforms) 1) (not (zero? distance)))
                   (m/loft (mapv (fn [tf] {:cross-section cross-section :frame tf})
                                 (if (neg? distance) (vec (rseq transforms)) transforms))))]
    (update (if branch? frame end) :segments conj
            (with-meta (assoc (Segment. (first transforms) (:segment-transform end) transforms cross-section manifold true)
                              :surface-transforms (mapv #(surface/frame % axis) samples))
              (merge (meta frame) props)))))

(defn extrude*
  ([forms]
   (extrude* {:current-frame-ids #{}
              :default-frame ::default-frame
              :forms forms
              :angle-scalar 57.29578
              :models {}}
             forms
             {::default-frame {:frame-transform (tf/transform)
                               :segment-transform (tf/transform)
                               :segments []}}))
  ([state forms frames]
   (let [current-form (atom nil)]
     (try
       (loop [{:keys [current-frame-ids angle-scalar] :as state} state
              [form & forms] forms
              frames frames
              result-forms []]
         (if (nil? form)
           (map->Extrusion
            {:result-forms result-forms
             :frames (dissoc frames ::default-frame)
             :state state
             :angle-scalar angle-scalar
             :forms (:forms state)
             :transforms (:transforms state)
             :models (:models state)})
           (let [_ (reset! current-form form)
                 form (resolve-form form)]
             (reset! current-form form)
             (if (or (nil? form) (sequential? form))
               (recur state (concat (normalize-segment form) forms) frames result-forms)
               (case (:op form)
               :plexus.impl/frame
               (let [frame-id (:name form)
                     replace (:replace form)
                     form (if replace
                            (assoc form :cross-section (:cross-section (get frames replace)))
                            form)
                     default-frame-id (:default-frame state)
                     default-frame (get frames default-frame-id)
                     default-frame-transform (m/compose-frames
                                              (:frame-transform default-frame)
                                              (:segment-transform default-frame))
                     frame (with-meta
                             (map->Frame (merge (if-let [frame (get frames frame-id)]
                                                  (assoc frame
                                                         :segment-transform
                                                         (m/compose-frames
                                                            (m/invert-frame (:frame-transform frame))
                                                            default-frame-transform))
                                                  (assoc default-frame
                                                         :segments []
                                                         :segment-transform (m/frame 1)
                                                         :frame-transform default-frame-transform))
                                          (select-keys form [:name :cross-section :curve-radius])))
                             (or (:meta form) {}))
                     frame (if-let [cursor (:surface-cursor default-frame)]
                             (surface-frame frame cursor)
                             (dissoc frame :surface-cursor))]
                 (recur (cond-> (-> state
                                    (assoc :default-frame frame-id)
                                    (update :current-frame-ids conj frame-id))
                          replace (update :current-frame-ids disj replace)
                          )
                        forms
                        (assoc frames frame-id frame)
                        result-forms))

               :plexus.impl/on-surface
               (let [prepared (if (surface/surface? (:surface form)) (:surface form)
                                  (surface/prepare (:surface form)))
                     cursor (surface/attach prepared form)
                     apply-to (if-let [to (:to form)] (set to)
                                  (conj current-frame-ids (:default-frame state)))]
                 (recur state forms
                        (reduce (fn [frames id]
                                  (when-not (get frames id)
                                    (throw (ex-info "Unknown surface frame" {:frame id})))
                                  (update frames id surface-frame cursor)) frames apply-to)
                        result-forms))

               :plexus.impl/off-surface
               (let [apply-to (if-let [to (:to form)] (set to)
                                  (conj current-frame-ids (:default-frame state)))]
                 (recur state forms
                        (reduce (fn [frames id]
                                  (when-not (get frames id)
                                    (throw (ex-info "Unknown surface frame" {:frame id})))
                                  (update frames id dissoc :surface-cursor)) frames apply-to)
                        result-forms))

               :plexus.impl/forward
               (let [{:keys [length x y z to n-steps transform-step-fn
                             twist props gap center branch? model]
                      :or {transform-step-fn (fn [tf i] tf)}} form
                     axis (cond x :x y :y :else :z)
                     length (or length x y z)
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames
                                    frame-id
                                    (if (:surface-cursor frame)
                                      (surface-forward frame form frame-id axis length)
                                      (let [start-transform (cond-> (:segment-transform frame)
                                                            center (m/translate [0 0 (- (/ length 2))])
                                                            (= axis :x) (tf/rotate :x (/ u/pi 2))
                                                            (= axis :y) (tf/rotate :y (- (/ u/pi 2))))
                                          step-count (or n-steps 1)
                                          end-transform (-> (:segment-transform frame)
                                                            (tf/go-forward (cond-> length center (/ 2)) axis)
                                                            (transform-step-fn step-count))
                                          step-length (if n-steps (/ length n-steps) length)
                                          all-transforms (conj (vec (for [step (range step-count)]
                                                                      (-> (tf/go-forward start-transform (* step step-length) axis)
                                                                          (transform-step-fn step))))
                                                               end-transform)
                                          is-gap (or (true? gap)
                                                     (and (sequential? gap) (contains? (set gap) frame-id)))
                                          cross-section (and (not is-gap) (:cross-section frame))
                                          extrusion (or model (when cross-section
                                                                (m/extrude cross-section length)))]
                                      (-> frame
                                          (assoc :segment-transform end-transform)
                                          (update :segments
                                                  conj
                                                  (with-meta
                                                    (Segment. start-transform end-transform all-transforms cross-section
                                                              (when extrusion
                                                                (m/transform extrusion start-transform))
                                                              true)
                                                    (merge (meta frame) props)))))))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/set
               (let [{:keys [to]} form
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (update frames frame-id merge (dissoc form :to :op))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/set-meta
               (let [{:keys [to]} form
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames frame-id (vary-meta frame merge (dissoc form :to :op)))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/offset
               (let [{:keys [delta to join-type simplify]
                      :or {join-type :square}} form
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames
                                    frame-id
                                    (cond-> frame
                                      true (update :cross-section m/offset delta join-type)
                                      simplify (update :cross-section m/simplify simplify)))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/loft
               (let [{:keys [to algorithm]
                      :or {algorithm :eager-nearest-neighbor}} form
                     loft-forms (normalize-segment (:plexus.impl/list form))
                     loft-frames (or to current-frame-ids)
                     ret (extrude* state loft-forms frames)]
                 (recur (:state ret)
                        forms
                        (reduce
                         (fn [ret-frames frame-id]
                           (let [before-frame (get frames frame-id)
                                 ret-frame (get ret-frames frame-id)]
                             (assoc ret-frames
                                    frame-id
                                    (update ret-frame
                                            :segments
                                            (fn [segments]
                                              (let [n-segments-before-loft (count (:segments before-frame))
                                                    loft-segments (subvec segments n-segments-before-loft)
                                                    prev-segments (subvec segments 0 n-segments-before-loft)
                                                    first-segment (first loft-segments)
                                                    last-segment (peek loft-segments)
                                                    start-tf (:start-transform (first loft-segments))]
                                                (conj prev-segments
                                                      (with-meta
                                                        (Segment. start-tf (:end-transform (peek loft-segments)) [] nil
                                                                  (m/loft
                                                                   (cons
                                                                    {:frame (-> first-segment :all-transforms (nth 0))
                                                                     :algorithm algorithm
                                                                     :cross-section (or (:cross-section before-frame)
                                                                                        (:cross-section first-segment))}
                                                                    (mapcat
                                                                     (fn [{:keys [cross-section all-transforms]}]
                                                                       (for [tf (if (> (count all-transforms) 1)
                                                                                  (next all-transforms)
                                                                                  all-transforms)]
                                                                         {:cross-section cross-section
                                                                          :frame tf}))
                                                                     (remove nil? (filter :cross-section loft-segments)))))
                                                                  true)
                                                        (meta ret-frame)))))))))
                         (:frames ret)
                         loft-frames)
                        (into result-forms (:result-forms form))))

               (:plexus.impl/left :plexus.impl/right :plexus.impl/up :plexus.impl/down)
               (let [{:keys [curve-radius angle to op transform-step-fn props gap cs arc-length]
                      :or {transform-step-fn (fn [tf angle] tf)
                           cs 20}} form
                     apply-to (if to (set to) current-frame-ids)
                     angle (if angle
                             angle
                             (/ arc-length curve-radius))]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)
                                 _ (when (:surface-cursor frame)
                                     (throw (ex-info "Surface curves require alternating rotate :y and forward; use off-surface for spatial curves" {})))
                                 curve-radius (or curve-radius
                                                  (:curve-radius frame)
                                                  (throw (ex-info "No :curve-radius defined for frame or segment." {})))
                                 start-transform (:segment-transform frame)
                                 d (u/bAc->a curve-radius angle curve-radius)
                                 r (- (/ u/pi 2) (/ (- u/pi angle) 2))
                                 sign (case op (:plexus.impl/left :plexus.impl/up) - +)
                                 f (case op (:plexus.impl/up :plexus.impl/down) tf/pitch tf/yaw)
                                 end-transform (-> start-transform
                                                   (f (sign r))
                                                   (tf/go-forward d)
                                                   (f (sign (- angle r))))
                                 is-gap (or (true? gap)
                                            (and (sequential? gap) (contains? (set gap) frame-id)))
                                 cross-section (and (not is-gap) (:cross-section frame))
                                 all-tfs (if gap
                                           []
                                           (all-transforms start-transform
                                                           (fn [tf r d a]
                                                             (-> tf
                                                                 (f (sign r))
                                                                 (tf/go-forward d)
                                                                 (f (sign (- a r)))))
                                                           curve-radius
                                                           angle
                                                           cs
                                                           transform-step-fn))
                                 manifold (when cross-section
                                            (-> cross-section
                                                (m/rotate 180)
                                                (cond-> (= op :plexus.impl/up) (m/rotate 90)
                                                        (= op :plexus.impl/down) (m/rotate -90)
                                                        (= op :plexus.impl/right) (m/rotate 180))
                                                (m/mirror [1 0])
                                                (m/translate [curve-radius 0])
                                                (m/revolve cs (* angle angle-scalar))
                                                (m/translate [(- curve-radius) 0 0])
                                                (m/rotate [90 0 0])
                                                (m/rotate [0 0 (case op
                                                                 :plexus.impl/right 180
                                                                 :plexus.impl/up -90
                                                                 :plexus.impl/down 90
                                                                 0)])
                                                (m/transform start-transform)))]
                             (assoc frames
                                    frame-id
                                    (-> frame
                                        (assoc :segment-transform end-transform)
                                        (update :segments
                                                conj
                                                (with-meta
                                                  (Segment. start-transform end-transform all-tfs cross-section manifold true)
                                                  (merge (meta frame) props)))))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/translate
               (let [{:keys [x y z to global?]} form
                     apply-to (if to
                                (set to)
                                (conj current-frame-ids (:default-frame state)))]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames
                                    frame-id
                                    (if-let [cursor (:surface-cursor frame)]
                                      (do
                                        (when global?
                                          (throw (ex-info "Global translation requires off-surface first" {})))
                                        (surface-frame frame (surface/move cursor (or x 0) (or y 0) (or z 0))))
                                      (if global?
                                        (update frame :segment-transform #(tf/set-translation % [(or x 0) (or y 0) (or z 0)]))
                                        (update frame :segment-transform m/translate [(or x 0) (or y 0) (or z 0)]))))))
                         frames
                         apply-to)
                        result-forms))


               :plexus.impl/rotate
               (let [{:keys [x y z to]} form
                     apply-to (if to
                                (set to)
                                (conj current-frame-ids (:default-frame state)))]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames
                                    frame-id
                                    (if-let [cursor (:surface-cursor frame)]
                                      (do
                                        (when (or (not (zero? (or x 0))) (not (zero? (or z 0))))
                                          (throw (ex-info "Surface frames turn with rotate :y; use off-surface before tilting" {})))
                                        (surface-frame frame (surface/turn cursor (or y 0))))
                                      (update frame :segment-transform m/rotate [(* 1 (or x 0))
                                                                               (* 1 (or y 0))
                                                                               (* 1 (or z 0))])))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/segment
               (let [segment-forms (normalize-segment (:plexus.impl/list form))]
                 (recur state
                        (concat segment-forms forms)
                        frames
                        result-forms))

               :plexus.impl/hull
               (let [{:keys [to]} form
                     hull-forms (normalize-segment (:plexus.impl/list form))
                     hull-frames (or to current-frame-ids)
                     ret (extrude* state hull-forms frames)]
                 (recur (:state ret)
                        forms
                        (reduce
                         (fn [ret-frames frame-id]
                           (let [before-frame (get frames frame-id)
                                 ret-frame (get ret-frames frame-id)]
                             (assoc ret-frames
                                    frame-id
                                    (update ret-frame
                                            :segments
                                            (fn [segments]
                                              (let [n-segments-before-hull (count (:segments before-frame))
                                                    hull-segments (subvec segments n-segments-before-hull)
                                                    prev-segments (subvec segments 0 n-segments-before-hull)
                                                    start-transform (:start-transform (first hull-segments))
                                                    end-transform (:end-transform (peek hull-segments))
                                                    manifold (apply m/hull (remove nil? (map :manifold hull-segments)))]
                                                (conj prev-segments (with-meta
                                                                      (Segment. start-transform end-transform [] nil manifold true)
                                                                      (meta ret-frame)))))))))
                         (:frames ret)
                         hull-frames)
                        (into result-forms (:result-forms ret))))

               :plexus.impl/branch
               (let [{:keys [from with]} form
                     _ (when-not (get frames from)
                         (throw (ex-info (str "`:from` frame `" from "` does not exist in frame set: "
                                      (vec (keys (dissoc frames ::default-frame))))
                                         {})))
                     with-frames (or with current-frame-ids)
                     branch-ret (extrude* (assoc state
                                                 :default-frame from
                                                 :current-frame-ids (into #{} with-frames))
                                          (normalize-segment (:plexus.impl/list form))
                                          frames
                                          #_(select-keys frames (conj with-frames from)))]
                 (recur (-> state
                            (update :transforms #(into (or % {}) (:transforms branch-ret)))
                            (update :models merge (:models branch-ret)))
                        forms
                        (reduce-kv
                         (fn [ret frame-id branch-frame]
                           (if-let [frame (get ret frame-id)]
                             (assoc ret
                                    frame-id
                                    (assoc frame
                                           :segments (:segments branch-frame)))
                             (assoc ret frame-id branch-frame)))
                         frames
                         (:frames branch-ret))
                        (into result-forms (:result-forms branch-ret))))


               :plexus.impl/result
               (recur state forms frames (conj result-forms form))


               :plexus.impl/add-ns
               (let [{:keys [namespace to]} form
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)]
                             (assoc frames
                                    frame-id
                                    (update frame
                                            :namespace
                                            #(keyword
                                              (if %1
                                                (str (name %1) "." (name namespace))
                                                namespace))))))
                         frames
                         apply-to)
                        result-forms))

               :plexus.impl/save-transform
               (let [{:keys [frame to name ref]} form
                     frame (get frames frame)
                     namespace (:namespace frame)
                     transform-name (cond->> (or name "default")
                                      namespace (->keyword namespace))
                     saved-frame (m/compose-frames
                                  (:frame-transform frame)
                                  (:segment-transform frame))]
                 (when ref
                   (reset! ref saved-frame))
                 (recur (update state :transforms assoc transform-name saved-frame)
                        forms
                        frames
                        result-forms))

               :plexus.impl/transform
               (let [{:keys [replace to]} form
                     apply-to (if to (set to) current-frame-ids)]
                 (recur state
                        forms
                        (reduce
                         (fn [frames frame-id]
                           (let [frame (get frames frame-id)
                                 _ (when (:surface-cursor frame)
                                     (throw (ex-info "Replacing a surface transform requires off-surface first" {})))]
                             (assoc frames
                                    frame-id
                                    (assoc frame
                                           :segment-transform
                                           (m/compose-frames
                                            (m/invert-frame (:frame-transform frame))
                                            replace)))))
                         frames
                         (conj apply-to (:default-frame state)))
                        result-forms))

               :plexus.impl/insert
               (let [{:keys [extrusion models at ns end-frame]} form
                     _ (when (and end-frame (some :surface-cursor (vals frames)))
                         (throw (ex-info "insert :end-frame requires off-surface first" {})))
                     default-frame-id (:default-frame state)
                     at (or at default-frame-id)
                     base-frame (get frames at)
                     base-frame-transform (m/compose-frames
                                           (:frame-transform base-frame)
                                           (:segment-transform base-frame))
                     extrusion-models (:models extrusion)
                     _ (let [missing-models (remove #(contains? extrusion-models %) models)]
                         (when (seq missing-models)
                           (throw (ex-info (str "Insertion models not defined in extrusion: " (vec missing-models)) {}))))
                     insert-end-frame (if end-frame
                                        (-> extrusion :frames end-frame)
                                        base-frame)
                     end-transform (m/compose-frames
                                    (:frame-transform insert-end-frame)
                                    (:segment-transform insert-end-frame))
                     current-models (:models state)]
                 (recur (update state :models into
                                (map (fn [model-id]
                                       (let [model (m/transform (get extrusion-models model-id) base-frame-transform)
                                             full-model-id (if ns
                                                             (keyword (name ns) (name model-id))
                                                             model-id)]
                                         [full-model-id
                                          (if-let [current-model (get current-models full-model-id)]
                                            (m/union current-model model)
                                            model)]))
                                     (or models [(:main-model extrusion)])))
                        forms
                        (if end-frame
                          (reduce
                           (fn [frames frame-id]
                             (let [frame (get frames frame-id)]
                               (assoc frames
                                      frame-id
                                      (assoc frame
                                             :segment-transform
                                             (m/compose-frames
                                              end-transform
                                              (:segment-transform frame))))))
                           frames
                           (conj current-frame-ids default-frame-id))
                          frames)
                        result-forms))

               (throw (ex-info (str "No matching clause for form: " form) {:form form})))))))
       (catch #?(:clj Exception :cljs :default) e
         (throw (error-with-context e @current-form (meta @current-form))))))))

(defn extrude [forms]
  (let [result (extrude* (normalize-segment forms))
        frames (:frames result)
        angle-scalar (:angle-scalar result)
        result-forms (:result-forms result)
        unioned-frames (merge frames (:models result))
        _ (when (empty? unioned-frames)
            (throw (ex-info "No frames included in extrusion." {})))
        main-model-name (or (:name (first result-forms))
                            (key (first unioned-frames)))
        model-cache (atom {})
        to-manifold-cached (fn [x]
                             (or (get @model-cache x)
                                 (let [ret (to-manifold x)]
                                   (swap! model-cache assoc x ret)
                                   ret)))]
    (-> result
        (assoc :main-model main-model-name)
        (assoc :models
               (persistent!
                (reduce-kv
                 (fn [ret k v]
                   (if (m/manifold? v)
                     (assoc! ret k v)
                     (if-let [manifold (to-manifold v)]
                       (assoc! ret k manifold)
                       ret)))
                 (transient {})
                 (reduce (fn [result-frames result-form]
                           (assoc result-frames
                                  (:name result-form)
                                  ((fn eval-result [expr & [parent-source]]
                                     (let [source (merge (meta result-form) parent-source (meta expr))]
                                     (try
                                     (cond (or (keyword? expr) (string? expr))
                                           (if-let [frame_ (get result-frames expr)]
                                             frame_
                                             (throw (ex-info (str "Frame is not defined: " expr) {:expr expr})))
                                           (m/manifold? expr)
                                           expr
                                           :else
                                           (let [expr (resolve-form expr)
                                                 args (normalize-segment (::list expr))]
                                             (case (:op expr)
                                               :plexus.impl/hull
                                               (m/hull (map to-manifold-cached (map #(eval-result % source) args)))

                                               :plexus.impl/mirror
                                               (let [ret (eval-result (first args) source)]
                                                 (cond (frame? ret) (update (to-model ret) :manifold m/mirror (:normal expr))
                                                       (model? ret) (update ret :manifold m/mirror (:normal expr))
                                                       (m/manifold? ret) (m/mirror ret (:normal expr))))

                                               :plexus.impl/translate
                                               (let [ret (eval-result (first args) source)
                                                     tr [(or (:x expr) 0)
                                                         (or (:y expr) 0)
                                                         (or (:z expr) 0)]]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/translate tr)

                                                       (m/manifold? ret) (m/translate ret tr)))

                                               :plexus.impl/rotate
                                               (let [ret (eval-result (first args) source)]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/rotate
                                                                                              [(or (:x expr) 0)
                                                                                               (or (:y expr) 0)
                                                                                               (or (:z expr) 0)]                                    )

                                                       (m/manifold? ret) (m/rotate ret [(* angle-scalar (or (:x expr) 0))
                                                                                        (* angle-scalar (or (:y expr) 0))
                                                                                        (* angle-scalar (or (:z expr) 0))])))

                                               :plexus.impl/union
                                               (m/union (map to-manifold-cached (map #(eval-result % source) args)))

                                               :plexus.impl/difference
                                               (m/difference (map to-manifold-cached (map #(eval-result % source) args)))

                                               :plexus.impl/intersection
                                               (m/intersection (map to-manifold-cached (map #(eval-result % source) args)))

                                               :plexus.impl/trim-by-plane
                                               (let [ret (eval-result (first args) source)
                                                     normal (:normal expr)
                                                     origin-offset (or (:origin-offset expr) 0)]
                                                 (m/trim-by-plane ret normal origin-offset))

                                               (throw (ex-info (str "Unknown expr: " expr) {:expr expr})))))
                                     (catch #?(:clj Exception :cljs :default) e
                                       (throw (error-with-context e expr (assoc source :op ::result)))))))
                                   (:expr result-form))))
                         unioned-frames
                         (rseq result-forms))))))))

(defn path-points
  ([extrusion]
   (path-points extrusion identity nil))
  ([extrusion select-fn meta-props]
   (vec (for [[_ frame] (:frames extrusion)
              [i seg] (map-indexed list (:segments frame))
              :let [samples (or (:surface-transforms seg) (:all-transforms seg))
                    tfs (if (zero? i) samples (next samples))
                    meta-fn (if meta-props
                              (fn [x]
                                (with-meta x (select-keys (meta seg) meta-props)))
                              identity)]
              tf (->> tfs
                      (map tf/translation-vector)
                      (map select-fn)
                      (map meta-fn))]
          tf))))

(defn path-transforms
  ([extrusion]
   (vec (for [[_ frame] (:frames extrusion)
              [i seg] (map-indexed list (:segments frame))
              :let [samples (or (:surface-transforms seg) (:all-transforms seg))
                    tfs (if (zero? i) samples (next samples))]
              tf tfs]
          tf))))
