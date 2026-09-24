(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require #?(:clj [clojure.java.io :as io])
            [plexus.schema :as schema]
            [clj-manifold3d.core :as m]
            [plexus.impl :as impl]))

(defn deferred
  "Defer a zero-argument function returning a form or nested sequence of forms.
  Called once on each extrusion/rebuild, in the caller's dynamic bindings."
  [f]
  (when-not (fn? f)
    (throw (ex-info "deferred expects a zero-argument function" {:value f})))
  f)

(defn left
  "Extrude a left curve. Rotate-extrudes about a y-axis offset
  along the x axis by negative `:curve-radians`.

  opts
  `:angle` - number of radians to curve.
  `:curve-radius` - radius of the curve.
  `:arc-length` - Arc length of the curve (exclusive with `:angle`)."
  [& opts]
  (schema/validate-form (cons :plexus.impl/left opts) schema/curve-schema))

(defn right
  "Extrude an right curve. Rotate-extrudes about a y-axis offset
  along the x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (schema/validate-form (cons :plexus.impl/right opts) schema/curve-schema))

(defn up
  "Extrude a up curve. Rotate-extrudes about a x-axis offset
  along the y axis by negative `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (schema/validate-form (cons :plexus.impl/up opts) schema/curve-schema))

(defn down
  "Extrude a right curve. Rotate-extrudes about a x-axis offset
  along the x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to curve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (schema/validate-form (cons :plexus.impl/down opts) schema/curve-schema))

(defn curve
  "Extrude a curve segment.

  opts
  `:angle` number of radians to curve.
  `:curve-radius` radius of the curve.
  `:direction` :left, :right, :up, or :down direction. Default is :left.
  `:roll` number of radians to roll aboute Z before extruding."
  [& opts]
  (schema/validate-form (cons :plexus.impl/curve opts) schema/curve-generic-schema))

(defn forward
  "Extrude the model set forward, relative to current frame.

  opts:

  `:length` - Extrude in canonical direction (along z axis)
  `:x` - extrude along x axis
  `:y` - extrude along y axis
  `:z` - extrude along z axis
  `:twist` - Twist radians while extruding. End transform matches the degree of twist.
  `:center` - (default: false) whether extrusion should be centered at starting transform frame.
  `:branch?` - (default: false) whether extrusion should be applied in a branch without updating
               the transform.
  `:n-steps` - Number of steps in the extrusion.
  `:transform-step-fn` - A function (fn [tf i] tf) applied to sampled frames, including the final
                        frame at i = n-steps (or 1). The final frame becomes the next segment's
                        starting frame. Use `loft` to build curved geometry from these samples;
                        this does not bend the ordinary straight extrusion. "
  [& opts]
  (schema/validate-form (cons :plexus.impl/forward opts) schema/forward-schema))

(defn loft
  "Loft from the entering frame's cross-section through the segment endpoints.
  Setting a cross-section inside the loft changes the following segment endpoint.
  All lofted cross-sections must have equal number of points. Optional
  `:to` parameter specifies which frames to loft."
  [& opts]
  (schema/validate-form (cons :plexus.impl/loft opts) schema/loft-schema))

(defn hull
  "Makes a convex hull out of wrapped segments. Optional `:to` parameter specifies which
  frames to hull."
  [& opts]
  (schema/validate-form (cons :plexus.impl/hull opts) schema/any-map-schema))

(defn translate
  "Translate frame. Takes `:x`, `:y`, and/or `:z` parameters and applies the translation
  relative to the frame transform. If :global? is set, axis values set relative to global origin."
  [& opts]
  (schema/validate-form (cons :plexus.impl/translate opts) schema/translate-schema))

(defn mirror
  "Only applies to result expressions.

  opts:

  `:normal` [x y z] normal to the mirror plane."
  [& opts]
  (schema/validate-form (cons :plexus.impl/mirror opts) schema/mirror-schema))

(defn rotate
  "Rotates around `:x`, `:y`, and/or `:z` axes relative to the current frame(s) transformation."
  [& opts]
  (schema/validate-form (cons :plexus.impl/rotate opts) schema/rotate-schema))

(defn transform
  "Currently only supports `:replace` parameter, which replaces the current frames transformation."
  [& opts]
  (schema/validate-form (cons :plexus.impl/transform opts) schema/transform-schema))

(defn set
  "Set default args for active frames, or explicity with the `:to [:frame1 :frame2 ...]` parameter."
  [& opts]
  (schema/validate-form (cons :plexus.impl/set opts) schema/any-map-schema))

(defn set-meta
  "Set meta-data on the current frame. Segments inherent metadata from the frame they are associated with."
  [& opts]
  (schema/validate-form (cons :plexus.impl/set-meta opts) schema/any-map-schema))

(defn branch
  "Branch off from a frames current transform.

  opts:
 `:from` (required) parameter specifies which frame to branch off from.
         The body of the branch is the same as (extrude ...).
  `:with` (optional) Specifies what subset of current frames to include in the
          branch context."
  [& opts]
  (schema/validate-form (cons :plexus.impl/branch opts) schema/branch-schema))

(defn to
  "Apply nested forms only to the frames in :models."
  [& forms]
  (let [[opts forms] (impl/parse-path forms)]
    (when-not (and (sequential? (:models opts)) (every? #(or (keyword? %) (string? %)) (:models opts)))
      (throw (ex-info "to requires :models, a sequence of frame names" {:options opts})))
    (letfn [(target [form]
              (let [form (impl/resolve-form form)]
                (cond (nil? form) nil
                      (sequential? form) (map target form)
                      :else (assoc form :to (:models opts)))))]
      (deferred #(map target forms)))))

(defn frame
  "Add a new frame to the current context.

   opts
   `:name` (required) A keyword or string naming the frame.
   `:cross-section` (optional) Cross-section to associate with the
                     current frame."
  [& opts]
  (schema/validate-form (cons :plexus.impl/frame opts) schema/frame-schema))

(defn save-transform
  "Save transform by name.

  opts

  `:frame` - The frame to save the current transform of.
  `:name` - Name of the saved transform."
  [& opts]
  (schema/validate-form (cons :plexus.impl/save-transform opts) schema/save-transform-schema))

(defn offset
  "offset cross-sections.

  opts

  `:deta` - number of mm to offset, positive or negative.
  `:join-type` - Type of corners to create when offsetting. Options are :square, :round, or :miter.
  `:simplify` - Remove vertices from resulting contours in cross-section that are less than the
                specified distance epsilon from an imaginary line that passes through its two adjacent vertices."
  [& opts]
  (schema/validate-form (cons :plexus.impl/offset opts) schema/offset-schema))

(defn add-ns
  "Namespace frames. If a namespace already exists, it will be be extended (period separated).

  opts

  `:namespace` - string or keyword."
  [& opts]
  (schema/validate-form (cons :plexus.impl/add-ns opts) schema/add-ns-schema))

(defn result
  "Combines frames together with CSG and other 3D geometric operations. Supports operations
  `union`, `difference`, `intersection`, `translate`, `rotate`, `mirror`, `loft`, `hull`,
   and `trim-by-plane`. Frames in result trees are specified by name. Results can also include
   other results (also by name).

  opts

  :name - name of the result.
  :expr - Tree of SCG and other 3D geometric operations."
  [& opts]
  (schema/validate-form (cons :plexus.impl/result opts) schema/result-schema))

(defn union
  "For use in result expressions. Unions frames."
  [& opts]
  (schema/validate-form (cons :plexus.impl/union opts) schema/result-op-schema))

(defn intersection
  "For use in result expressions. Intersects frames."
  [& opts]
  (schema/validate-form (cons :plexus.impl/intersection opts) schema/result-op-schema))

(defn difference
  "For use in result expressions. Subtracts frames."
  [& opts]
  (schema/validate-form (cons :plexus.impl/difference opts) schema/result-op-schema))

#_(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args) schema/import-schema))

(defn insert
  "insert an extrusion at the current position and orientation.

  opts

  `:extrusion` - extrusion to insert.
  `:models` (optional) - models to inserts. Can include results and/or frames. Defaults to singleton vector of last result model.
  `:ns` (optional) - an optional namespace ot associate with inserted models.
  `:end-frame` (optional) - name of a inserted the next segment is relative to. The frame must be defined in the provided extrusion."
  [& opts]
  (schema/validate-form (cons :plexus.impl/insert opts) schema/insert-schema))


#_(defn show-coordinate-frame [& args]
    (validate-form `(:plexus.impl/show-coordinate-frame ~@args) schema/show-coordinate-frames-schema))

(defn trim-by-plane
  "For use in result expressions. Trims a model by subracting the halfspace defined by the `:normal`.

  :opts

  `:normal` - [x y z] defines the normal to the plane, points in the direction of the subtracted halfspace.
  `:origin-offset` - Offset of the normal from [0 0 0] in the direction of the normal."
  [& opts]
  (schema/validate-form (cons :plexus.impl/trim-by-plane opts) schema/any-map-schema))

(defn lookup-transform
  "lookup saved transform by name. See `save-transform`."
  [extrusion key]
  (get (:transforms extrusion) key))

(defn get-frame
  "Get frame from `extrusion` by `name`"
  [extrusion name]
  (get (:frames extrusion) name))

(defn get-model
  "Get model from `extrusion` by `name`. Models are composited frames. For every frame, a model
  is created by unioning the segments. Also, every result expression yields a model."
  [extrusion name]
  (get (:models extrusion) name))

(defn extrude
  "the main entry point. Takes a sequence of extrusion forms. Forms are automatically flattened."
  [& forms]
  (impl/extrude forms))

(defn rebuild
  "Rebuild using the saved forms. Values are captured at construction; use `deferred`
  to re-evaluate selected forms in the current dynamic bindings."
  [extrusion]
  (extrude (:forms extrusion)))

(def extrusion? impl/extrusion?)
(def model? impl/model?)
(def frame? impl/frame?)

(defn points
  "Return path points, projected onto :axes (default [:x :y])."
  [& forms]
  (let [[opts forms] (impl/parse-path forms)
        axes (or (:axes opts) [:x :y])
        indices (mapv {:x 0 :y 1 :z 2} axes)]
    (when (some nil? indices)
      (throw (ex-info "Point axes must be :x, :y, or :z" {:axes axes})))
    (impl/path-points (impl/extrude forms)
                      (fn [point] (mapv #(nth point %) indices))
                      (:meta-props opts))))

#?(:clj
   (defmacro defmodel [name & path]
     `(def ~name (extrude ~@path))))

#?(:clj
   (defn export-models
     "Export Manifold and Extrusion vars in `namespace` using `file-ext` format."
     ([namespace file-ext]
      (export-models namespace file-ext #".*" ""))
     ([namespace file-ext match-str]
      (export-models namespace file-ext match-str ""))
     ([namespace file-ext match-str name-postfix]
      (-> (io/file (format "out/%s" file-ext)) (.mkdirs))
      (doall
       (for [[_ x] (ns-map namespace)
             :let [var-meta (meta x)
                   var-name (str (:name var-meta))]
             :when (and (:export-model var-meta) (re-matches match-str var-name))]
         (future
           (let [filename (-> var-meta :name name (str name-postfix (format ".%s" file-ext)))
                 m @x]
             (if (m/manifold? m)
               (m/export-mesh (m/get-mesh (cond-> m (= file-ext "3ds") (m/rotate [-90 0 0]))) (format "out/%s/%s" file-ext filename))
               (let [manifold (cond-> (get (:models m) (:main-model m))
                                (#{"3ds"} file-ext) (m/rotate [-90 0 0]))]
                 (m/export-mesh (m/get-mesh manifold) (format "out/%s/%s" file-ext filename)))))))))))

(defn export
  ([model filename]
   (export model filename (m/material)))
  ([model filename material]
   (m/export-mesh (m/get-mesh (impl/to-manifold (cond-> model (m/cross-section? model) (m/extrude 0.5)))) filename :material material)))

(defn transforms
  [& forms]
  (impl/path-transforms (impl/extrude (cons (frame :name :origin) forms))))
