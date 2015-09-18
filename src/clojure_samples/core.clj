(ns clojure-samples.core
  (:require
            [clojure.string :refer [join]]
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(def my-data '({:sopt_days "SOPT Days", :business_unit "Business Unit" :vendor_sku "Vendor Sku" :sku "SKU"}{:sopt_days "12" , :business_unit "", :vendor_sku 55, :sku 99558858} {:sopt_days 25, :business_unit 1234, :vendor_sku 27, :sku 111111} {:sopt_days 11, :business_unit 1234, :vendor_sku 11, :sku 111111} {:sopt_days 10, :business_unit 1234, :vendor_sku 10, :sku 111111}))

(defn validate-records [x]
  (join ", "(remove nil? (list (if (not(number? (x :sopt_days))) "SOPT Days number validation error")
                               (if (not (number? (x :business_unit))) "Business unit number validation error")
                               (if (not (number? (x :vendor_sku))) "Vendor SKU validation error")
                               (if (not (number? (x :sku))) "SKU Validation Error")))))

(defn get-validation-errors
  [mydata]
  (let [err-msgs (into [] (for [x mydata]
                            (assoc x :err (validate-records x))))]
    (if (every? empty? (for [x err-msgs] (x :err))) (println "validation success") (do(println "validation fail")err-msgs))))

(def csv-error-header '("Error" "SOPT Days" "Business Unit" "Vendor SKU" "SKU"))
(def csv-header '("Error" "SOPT Days" "Business Unit" "Vendor SKU" "SKU"))


(def csv-header-map {:sopt_days "SOPT Days", :business_unit "Business Unit" :vendor_sku "Vendor Sku" :sku "SKU"})

(def sample-data `({:phone "9836966558", :name "Arindam", :role "001"} {:phone "", :name "Sutapa", :role "002"} {:phone "9836966559", :name "Puchki", :role "003"}))

(defn validate-headers
  [headers]
  (every? (first my-data) (keys headers)))

(defn- write-to-file
  [file-name data]
  (with-open [writer (io/writer file-name)]
    (io/.write writer data)))

(defn test-function []
  (let [vresult (get-validation-errors (rest my-data))]
    (if (every? nil? vresult) (println "Validation success" - vresult) (println "Validation failure" - vresult))
    (println (cons csv-error-header (for [x vresult] (map #(str %) (vals x)))))
    (println (csv/write-csv (cons csv-error-header (for [x vresult] (map #(str %) (vals x))))))
    (write-to-file "test.csv" (csv/write-csv (cons csv-error-header (for [x vresult] (map #(str %) (vals x))))))
    ;(println (csv/write-csv vresult))
    ))
;(let [new-data {:sopt_days 88, :business_unit "", :vendor_sku 99, :sku "9x9x9x9x"}]
;(println (cons (cons (first my-data) new-data) (rest my-data))))



(defn read-delimited-text-file
  "read a delimited text file, use delimiter of your choice"
  [file-name & {:as delimiter} ]
  (let [reader (io/reader file-name)
        delim (merge {:delimiter \~} delimiter)
        parsed-csv (csv/parse-csv reader :delimiter (:delimiter delim))]
    parsed-csv))

(defn write-delimited-text-file
  "write a delimited text file, use delimeter of your choice"
  [file-name data-set & {:as delimiter}]
  (let [delim (merge {:delimiter \,} delimiter)
        data-set-string (map (fn[x] (vals x)) data-set)
        csv-table (csv/write-csv data-set-string :delimiter (:delimiter delim))
        ]
    (write-to-file file-name csv-table)))

(defn mapify-data
  "create a map with your keys"
  [key-list data-list]
  (map (fn [x] (zipmap key-list x)) data-list))




(println (mapify-data `(":role" ":name" ":phone") (read-delimited-text-file "test.dat")))
(write-delimited-text-file "test-write.dat" sample-data :delimiter \~)


