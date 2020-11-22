;Eric Zhou, ezzhou
;BSL

;(1.)
;Signature: [String, String, Integer, Integer, Boolean] -> Restaurant Struct
;Purpose: A restaurant struct where:
;  name is a String of the restaurant name
;  food-type is a String of the restaurant's type of food
;  num-tables is a Number of the amount of tables
;  capacity is a Number of the max people able to be seated
;  vegan? is a Boolean that determines if a restaurant is vegan-friendly
(define-struct restaurant (name food-type num-tables capacity vegan?))

;Example restaurants:
(define Comella (make-restaurant "Comellas" "Italian" 5 25 #true))
(define McDonalds (make-restaurant "McDonald's" "Fast Food" 15 40 #false))
(define Yamato (make-restaurant "Yamato's" "Japanese" 50 300 #true))

(check-expect Comella (make-restaurant "Comellas" "Italian" 5 25 #true))

;(2.)
;define-restaurant creates the following functions:
;Constructor Signature: [String, String, Integer, Integer, Boolean] -> Restaurant Struct
;  make-restaurant [data 1...data n]
;Selector Signatures: a-restaurant -> dataType
;  (restaurant-name a-restaurant) -> String
;  (restaurant-food-type a-restaurant) -> String
;  (restaurant-num-tables a-restaurant) -> Number
;  (restaurant-capacity a-restaurant) -> Number
;  (restaurant-vegan? a-restaurant) -> Boolean
;Predicate Signature: anyValue -> Boolean
;  (restaurant? anyValue)

;(3.)
;Signature: Restaurant Struct -> String
;Purpose: Categorizing restaurants based on their information
(define (restaurant-type restaurant)
  (cond
    [(and (>= (restaurant-num-tables restaurant) 30) (>= (restaurant-capacity restaurant) 250)) "event venue"]
    [(restaurant-vegan? restaurant) "vegetarian-friendly"]
    [else (restaurant-food-type restaurant)]))

(check-expect (restaurant-type Comella) "vegetarian-friendly")
(check-expect (restaurant-type Yamato) "event venue")
(check-expect (restaurant-type McDonalds) "Fast Food")

;(4.)
;Signature: [Number Number Number] -> Date Struct
;Purpose: A date where (le bruh do you really need a explanation):
;  month is the month of the date
;  day is the day of the date
;  year is the year of the date
(define-struct date (month day year))

(define wait-wtf-hw1-is-due-when? (make-date 9 11 2020))
(define epoch (make-date 1 1 1970))
  
;Signature: [Restaurant String String Date Number] -> Reservation Struct
;Purpose: A reservation where:
;  a-restaurant is the name of the restaurant
;  reserver is the name of the person making the reservation
;  phone-number is the reserver's phone number
;  a-date is a the day of the reservation
;  num-people is the amount of people attending the reservation
(define-struct reservation (a-restaurant reserver phone-number a-date num-people))

(define R1 (make-reservation Comella "James" "6193242967" epoch 20))
(define R2 (make-reservation Yamato "Skylar" "7816178881" wait-wtf-hw1-is-due-when? 550))
(define R3 (make-reservation McDonalds "Juan" "1234567890" (make-date 7 17 2002) 8))

;(5.)
;Signature: [Reservation Number] -> Reservation
;Purpose: Too add a specified amount of people to an existing reservation
(define (add-to-party a-res num-add)
  (make-reservation (reservation-a-restaurant a-res) (reservation-reserver a-res) (reservation-phone-number a-res) (reservation-a-date a-res) (+ (reservation-num-people a-res) num-add)))

(check-expect (add-to-party R1 50) (make-reservation Comella "James" "6193242967" epoch 70))

;(6.)
;Signature [Date, Date] -> Boolean
;Purpose: To determine if the first date comes before the second date
(define (precedes? date1 date2)
  (and (<= (date-year date1) (date-year date2)) (<= (date-month date1) (date-month date2)) (< (date-day date1) (date-day date2))))

(check-expect (precedes? wait-wtf-hw1-is-due-when? epoch) #false)

;(7.)
;Signature: [Reservation, Date] -> String
(define (reservation-OK? a-res a-date)
  (cond
    [(precedes? (reservation-a-date a-res) a-date) "NOT OK"]
    [(> (reservation-num-people a-res) (restaurant-capacity (reservation-a-restaurant a-res))) "NOT OK"]
    [else "OK"]))

(check-expect (reservation-OK? R1 wait-wtf-hw1-is-due-when?) "NOT OK")
(check-expect (reservation-OK? R2 epoch) "NOT OK")
(check-expect (reservation-OK? R3 wait-wtf-hw1-is-due-when?) "OK")
