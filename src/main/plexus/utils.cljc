(ns plexus.utils)

(def pi #?(:clj Math/PI :cljs js/Math.PI))

(defn bAc->a
  "side,angle,side -> side via. law of cosines."
  [b A c]
  (#?(:clj Math/sqrt :cljs js/Math.sqrt) (- (+ (#?(:clj Math/pow :cljs js/Math.pow) b 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) c 2))
                (* 2 b c (#?(:clj Math/cos :cljs js/Math.cos) A)))))

(defn abc->ABC
  "side,side,side -> angle,angle,angle using Law of Cosines."
  [a b c]
  (let [C (#?(:clj Math/acos :cljs js/Math.acos) (/ (- (#?(:clj Math/pow :cljs js/Math.pow) c 2)
                           (#?(:clj Math/pow :cljs js/Math.pow) a 2)
                           (#?(:clj Math/pow :cljs js/Math.pow) b 2))
                        (- (* 2 a b))))
        B (#?(:clj Math/acos :cljs js/Math.acos) (/ (- (#?(:clj Math/pow :cljs js/Math.pow) b 2)
                           (#?(:clj Math/pow :cljs js/Math.pow) a 2)
                           (#?(:clj Math/pow :cljs js/Math.pow) c 2))
                        (- (* 2 a c))))
        A (- #?(:clj Math/PI :cljs js/Math.PI) (+ C B))]
    [A B C]))

(defn abc->A [a b c]
  (#?(:clj Math/acos :cljs js/Math.acos) (/ (- (#?(:clj Math/pow :cljs js/Math.pow) a 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) b 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) c 2))
                (- (* 2 b c)))))

(defn abc->B [a b c]
  (#?(:clj Math/acos :cljs js/Math.acos) (/ (- (#?(:clj Math/pow :cljs js/Math.pow) b 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) a 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) c 2))
                (- (* 2 a c)))))

(defn abc->C [a b c]
  (#?(:clj Math/acos :cljs js/Math.acos) (/ (- (#?(:clj Math/pow :cljs js/Math.pow) c 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) a 2)
                   (#?(:clj Math/pow :cljs js/Math.pow) b 2))
                (- (* 2 a b)))))

(defn abA->B [a b A]
  (#?(:clj Math/asin :cljs js/Math.asin) (/ (* b (#?(:clj Math/sin :cljs js/Math.sin) A)) a)))
