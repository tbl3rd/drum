;;234567890123456789012345678901234567890123456789012345678901234567890123456789

;; See: https://github.com/boot-clj/boot/wiki/Boot-Environment
;;
(set-env!
 :resource-paths #{"drum"}
 :target-path "target"
 :dependencies '[[gloss "0.2.4"]
                 [org.clojure/clojure "1.6.0"]]
 :main-class 'drum.core)

;;234567890123456789012345678901234567890123456789012345678901234567890123456789
