;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Project2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Author: Danielle Jayme Green
;Project 1 Functions in Scheme

;Finds the Modulus between two numbers
;@param num, demon  two number to do num%denom
(define myMod
  (lambda (num denom)
    (- num (* denom (quotient num denom)))
    )
)

;The main Sqrt function which the Project wanted
;but with only 1 parameter, needs to call helper
;function to work
(define mySqrt
  (lambda (x)
    (mySqrtHelper x 1)
    )
  )

;Helper function to mySqrt. Recursively calls itself
;with root + 1 to get the correct root
(define mySqrtHelper
  (lambda (x root)
    (if (< x (* root root))
        0
        (+ 1 (mySqrtHelper x (+ root 1)))
        )
    )
  )

;The Cubed Root function that will be called
;Uses 2 helper function depending on pos or neg input
(define myCbrt
  (lambda (x)
         (if (< x 0)
            (myCbrtHelperNeg x -1)
          (myCbrtHelperPos x 1)
          )
         )
  )

;The helper function for a negative square root
(define myCbrtHelperNeg
  (lambda (x root)
    (if (> x (* root root root))
        0
        (+ -1 (myCbrtHelperNeg x (- root 1)))
        )
    )
  )

;The helper function for a positive square root
(define myCbrtHelperPos
  (lambda (x root)
    (if (< x (* root root root))
        0
        (+ 1 (myCbrtHelperPos x (+ root 1)))
        )
    )
  )

;Finds the GCD of 2 numbers using the Euclid formula
(define myGcd
  (lambda (x y)
    (if (< y 1)
        x
        (myGcd y (myMod x y))
        )
    )
  )


;Finds the LCM of 2 numbers using formula using the GCD
(define myLcm
  (lambda (x y)
    (/ (* x y) (myGcd x y))
    )
  )


;Helper function for Average and other functions
;Adds every element of the list together
(define sumList
  (lambda (lst)
   (if (null? lst)
        0 
    (if (empty? lst)
        0
        (+ (car lst) (sumList (cdr lst)))
        )
    )
  )
  )


;Gets the size of the List and "returns" it
;Helper function for numerous functions
(define sizeOfList
  (lambda (lst)
    (if (empty? lst)
        0
        (+ 1 (sizeOfList(cdr lst)))
        )
    )
  )


;Searches the list to check if the value is in the list given
(define inList
  (lambda (lst x)
    (if (empty? lst)
        #f
        (if (= (car lst) x)
            #t
            (inList (cdr lst) x)
            )
        )
    )
  )


;Finds the max of a list and "returns" it
;Uses helper function myMaxHelper to find max
(define myMax
  (lambda (lst)
  (if (empty? lst)
      0
      (myMaxHelper lst 0)
      )
    )
  )


;Helper function of myMax which returns the max
(define myMaxHelper
  (lambda (lst max)
  (if (empty? lst)
      max
      (if (< max (car lst))
          (myMaxHelper (cdr lst) (car lst))
          (myMaxHelper (cdr lst) max)
          )
      )
    )
  )

;Finds the min of the lst given
;Utlizes helper function myMinHelper to do so
(define myMin
  (lambda (lst)
    (if (empty? lst)
        0
        (myMinHelper lst (myMax lst))
        )
    )
  )


;Finds the min 
(define myMinHelper
  (lambda (lst min)
    (if (empty? lst)
        min
        (if (> min (car lst))
            (myMinHelper (cdr lst) (car lst))
            (myMinHelper (cdr lst) min)
            )
        )
    )
  )


;Returns the average of the list given
;Utilizes sumList and sizeOfList helper functions
(define avgList
  (lambda (lst)
    (if (empty? lst)
        0
        (quotient (sumList lst) (sizeOfList lst))
        )
    )
  )


;Does a Scheme Selection Sort on the lst coming in
;Utilizes remove element and myMin in order to find, concatenate, and
; remove the smallest item
;Seriously, this took longer than the rest of the functions
(define mySort
  (lambda (lst)
    (if (null? lst)
        '()
        (if (empty? lst)
            '()
        (cons(myMin lst)(mySort(removeEle lst (myMin lst))))
        )
        )
    )
  )

;Removes the first instance of the number given from the list
;Helper function of mySort
(define removeEle
  (lambda (lst ele)
    (if (null? lst)
        '()
        (if (= (car lst) ele)
            (cdr lst)
            (cons (car lst) (removeEle (cdr lst) ele))
            )
        )
    )
  )


;Finds the median of the List
;Utilizes getElement (necessary) and medListOdd (to make it look better)
(define medList
  (lambda (lst)
    (if (empty? lst)
        0
        (if (< (myMod (sizeOfList lst) 2) 1)
            (quotient (+ (medListOdd lst) (getElement (mySort lst) (- (quotient (sizeOfList lst) 2) 1))) 2) ;Even Case
            (medListOdd lst) ;Odd sizeOfList
            )
        )
    )
  )

;Finds the easy odd (sizeOfList is odd) case of median
;Utilizes getElement
(define medListOdd
  (lambda (lst)
    (getElement (mySort lst) (quotient (sizeOfList lst) 2))
    )
  )

;Gets the element in the list at index given
;Helper function of the median list function
(define getElement
  (lambda (lst index)
    (if (= index 0)
        (car lst)
        (getElement(cdr lst)(- index 1))
        )
    )
  )