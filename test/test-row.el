(load "strings.el")

(ert-deftest test-grid-1-line-string-row ()
  (should
   (equal
    (concat "\n"
            (grid-make-row `((:content ,test-grid-1-line :width 20)
                             (:content ,test-grid-1-line :width 15 :align center)
                             (:content ,test-grid-1-line :width 20 :align right)))
)
    "
Fusce sagittis,     Fusce sagittis,     Fusce sagittis,
libero non molestie    libero non   libero non molestie
mollis, magna orci      molestie     mollis, magna orci
ultrices dolor, at   mollis, magna   ultrices dolor, at
vulputate neque      orci ultrices      vulputate neque
nulla lacinia eros.    dolor, at    nulla lacinia eros.
Aliquam erat        vulputate neque        Aliquam erat
volutpat.            nulla lacinia            volutpat.
                     eros. Aliquam                     
                     erat volutpat.                    "))
  (should
   (equal
    (concat "\n"
            (grid-make-row `((:content ,test-grid-1-line :width 20)
                             (:content ,test-grid-1-line :padding (1 . ?#)
                                       :width 15 :align center)
                             (:content ,test-grid-1-line :width 20 :align right))))
    "
Fusce sagittis,     ###############     Fusce sagittis,
libero non molestie #    Fusce    # libero non molestie
mollis, magna orci  #  sagittis,  #  mollis, magna orci
ultrices dolor, at  #  libero non #  ultrices dolor, at
vulputate neque     #   molestie  #     vulputate neque
nulla lacinia eros. #mollis, magna# nulla lacinia eros.
Aliquam erat        #orci ultrices#        Aliquam erat
volutpat.           #  dolor, at  #           volutpat.
                    #  vulputate  #                    
                    # neque nulla #                    
                    #lacinia eros.#                    
                    # Aliquam erat#                    
                    #  volutpat.  #                    
                    ###############                    ")))
