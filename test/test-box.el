;; -*- lexical-binding: t; -*-

(load "strings.el")

(ert-deftest test-grid-1-line-string-box ()
  "One-line string filled into 20-char box."
  (should
   (equal (concat "\n" (grid-make-box `(:content ,test-grid-1-line :width 20)))
          "
Fusce sagittis,     
libero non molestie 
mollis, magna orci  
ultrices dolor, at  
vulputate neque     
nulla lacinia eros. 
Aliquam erat        
volutpat.           "))
  (should
   (equal (concat "\n" (grid-make-box `( :content ,test-grid-1-line
                                         :width 20 :align right)))
          "
     Fusce sagittis,
 libero non molestie
  mollis, magna orci
  ultrices dolor, at
     vulputate neque
 nulla lacinia eros.
        Aliquam erat
           volutpat."))
  (should
   (equal (let ((s (grid-make-box `( :content ,test-grid-1-line
                                     :width 20 :border t))))
            (remove-text-properties 0 (length s) '(grid-box-uuid) s)
            s)
          #("Fusce sagittis,     
libero non molestie 
mollis, magna orci  
ultrices dolor, at  
vulputate neque     
nulla lacinia eros. 
Aliquam erat        
volutpat.           " 0 1 (face (:overline t :box (:line-width (-1 . 0))) grid-box-filled t) 1 15 (face (:overline t :box (:line-width (-1 . 0)))) 15 20 (face (:overline t :box (:line-width (-1 . 0)))) 21 40 (face (:box (:line-width (-1 . 0)))) 40 41 (face (:box (:line-width (-1 . 0)))) 42 60 (face (:box (:line-width (-1 . 0)))) 60 62 (face (:box (:line-width (-1 . 0)))) 63 81 (face (:box (:line-width (-1 . 0)))) 81 83 (face (:box (:line-width (-1 . 0)))) 84 99 (face (:box (:line-width (-1 . 0)))) 99 104 (face (:box (:line-width (-1 . 0)))) 105 124 (face (:box (:line-width (-1 . 0)))) 124 125 (face (:box (:line-width (-1 . 0)))) 126 138 (face (:box (:line-width (-1 . 0)))) 138 146 (face (:box (:line-width (-1 . 0)))) 147 156 (face (:box (:line-width (-1 . 0)) :underline (:position -3))) 156 167 (face (:box (:line-width (-1 . 0)) :underline (:position -3))))))
  (should
   (equal (concat "\n" (grid-make-box
                        `( :content ,test-grid-1-line :width 20
                           :align right :padding ((1 . ?-) (1 . ?|)))))
          "
|------------------|
|   Fusce sagittis,|
|        libero non|
|  molestie mollis,|
|        magna orci|
|ultrices dolor, at|
|   vulputate neque|
|     nulla lacinia|
|eros. Aliquam erat|
|         volutpat.|
|------------------|"))
  (should
   (equal (concat "\n" (grid-make-box
                        `( :content ,test-grid-1-line :width 20
                           :align center :padding ((1 . ?-) (1 . ?|)))))
          "
|------------------|
|  Fusce sagittis, |
|    libero non    |
| molestie mollis, |
|    magna orci    |
|ultrices dolor, at|
|  vulputate neque |
|   nulla lacinia  |
|eros. Aliquam erat|
|     volutpat.    |
|------------------|"))
  (should
   (equal
    (concat "\n" (grid-make-box
                  `( :content ,test-grid-1-line :width 20
                     :align center :padding (2 . ?#))))
    "
####################
####################
## Fusce sagittis,##
##   libero non   ##
##molestie mollis,##
##   magna orci   ##
## ultrices dolor,##
##  at vulputate  ##
##   neque nulla  ##
##  lacinia eros. ##
##  Aliquam erat  ##
##    volutpat.   ##
####################
####################")))
