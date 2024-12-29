(ns plexus.transforms
  (:import
   [manifold3d.linalg MatrixTransforms DoubleMat3x4 DoubleVec3 DoubleVec3]))

(defn yaw
  ([m a]
   (MatrixTransforms/Yaw m a)))

(defn pitch
  ([m a]
   (MatrixTransforms/Pitch m a)))

(defn roll
  ([m a]
   (MatrixTransforms/Roll m a)))

(defn transform? [x]
  (instance? DoubleMat3x4 x))

(defn rotate [m axis a]
  (MatrixTransforms/Rotate m
                           (case axis
                             :x (DoubleVec3. 1 0 0)
                             :y (DoubleVec3. 0 1 0)
                             :z (DoubleVec3. 0 0 1))
                           a))

(defn go-forward
  ([m x]
   (MatrixTransforms/Translate m (DoubleVec3. 0 0 x)))
  ([m x axis]
   (MatrixTransforms/Translate m (case axis
                                   :x (DoubleVec3. x 0 0)
                                   :y (DoubleVec3. 0 x 0)
                                   :z (DoubleVec3. 0 0 x)))))

(defn go-backward
  ([m x]
   (go-forward m (- x)))
  ([m x axis]
   (go-backward m (- x) axis)))

(defn set-translation
  ([m [x y z]]
   (DoubleMat3x4. (.getColumn ^DoubleVec3 m 0)
                  (.getColumn ^DoubleVec3 m 1)
                  (.getColumn ^DoubleVec3 m 2)
                  (DoubleVec3. x y z)))
  ([m v axis]
   (let [^DoubleVec3 tr (.getColumn m 3)]
     (DoubleMat3x4. (.getColumn ^DoubleVec3 m 0)
                    (.getColumn ^DoubleVec3 m 1)
                    (.getColumn ^DoubleVec3 m 2)
                    (case axis
                      :x (DoubleVec3. v (.y tr) (.z tr))
                      :y (DoubleVec3. (.x tr) v (.z tr))
                      :z (DoubleVec3. (.x tr) (.y tr) v))))))

(defn translation-vector [m]
  (let [tr (.getColumn m 3)]
    [(.x tr) (.y tr) (.z tr)]))

(def identity-tf
  (DoubleMat3x4/IdentityMat))

(defn transform
  ([]
   (DoubleMat3x4/IdentityMat)))
