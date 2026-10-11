(ns ejc-sql.structure-test
  (:require [clojure.test :refer :all]
            [ejc-sql.structure :refer :all]))

(deftest get-db-name-test
  (testing "get-db-name fn test."
    (is (= "my_db_name"
           (get-db-name
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= "my_db_name"
           (get-db-name
            {:connection-uri
             (str "jdbc:jtds:sqlserver://localhost:1433/my_db_name;"
                  "instance=instance;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= "my_db_name"
           (get-db-name {:database "my_db_name"
                         :subname "//localhost\\instance:1433"})))
    (is (= "my_db_name"
           (get-db-name {:subname "//localhost:3306/my_db_name"})))
    (is (= "my_db_name"
           (get-db-name {:subname "thin:@localhost:1521:my_db_name"})))))

(deftest get-user-test
  (testing "get-user fn test."
    (is (= "a_user"
           (get-user
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= :undefined-user
           (get-user
            {:connection-uri "jdbc:sqlite:/home/user/test.sqlite"})))))

(deftest get-db-type-test
  (testing "get-db-type fn test."
    (is (= :sqlserver
           (get-db-type
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= :sqlserver
           (get-db-type
            {:connection-uri
             (str "jdbc:jtds:sqlserver://localhost:1433/dbname;"
                  "instance=instance;"
                  "user=a_user;"
                  "password=secret;")})))))

(deftest get-colomns-candidates-test
  (testing "get-colomns-candidates fn test."
    (with-redefs-fn {#'get-tables (fn [db]
                                    '("users" "products"))
                     #'get-db-type (fn [db] :h2)
                     #'get-db-name (fn [db] "mydb")
                     #'get-all-tables get-tables
                     #'get-colomns (fn [db table force?]
                                     ({"users" '("id" "name")
                                       "products" '("id" "price")}
                                      table))}
      #(is
        (and
         (= '("nil" "id" "name")
            (let [sql "SELECT users. FROM users"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "users")))
         ;; Table alias
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users AS a"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "a")))
         ;; Table alias: spaces + tabs near to "AS"
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users  AS     a"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "a"))))))))

(deftest check-if-insert-sql-test
  (testing "insert-sql? fn test."
    (is (= "product"
           (insert-sql?
            "INSERT INTO product (name, price) VALUES ('socks', 1.25);")))
    (is (= nil
           (insert-sql?
            "UPDATE product SET price = 1.26 WHERE product = 'socks'")))
    (is (= nil
           (insert-sql?
            "SELECT * FROM product")))))

(deftest check-if-update-sql-test
  (testing "update-sql? fn test."
    (is (= nil
           (update-sql?
            "INSERT INTO product (name, price) VALUES ('socks', 1.25);")))
    (is (= "product"
           (update-sql?
            "UPDATE product SET price = 1.26 WHERE product = 'socks'")))
    (is (= nil
           (update-sql?
            "SELECT * FROM product")))))

(deftest from-tables-test
  (testing "from-tables fn test."
    (is (= ["product"]
           (from-tables "SELECT name FROM product WHERE id = 1")))
    (is (= ["product" "category"]
           (from-tables "SELECT  FROM product p, category c WHERE p.c = c.id")))
    (is (= ["product" "category" "price"]
           (from-tables (str "SELECT name\n"
                             "FROM public.product AS p\n"
                             "  JOIN category c ON c.id = p.category_id\n"
                             "  LEFT JOIN \"price\" pr ON pr.id = p.id\n"
                             "ORDER BY name"))))
    (is (= ["users"]
           (from-tables "SELECT  FROM (SELECT * FROM t) x JOIN users u ON 1=1")))
    (is (= []
           (from-tables "INSERT INTO product (name) VALUES ('socks')")))))
