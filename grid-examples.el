;; -*- lexical-binding: t; -*-

(defvar grid-multiline-str "line 1
line 2
line 3")

(defvar grid-lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

Ut enim ad minim veniam, quis nostrud exercitation ullamco

laboris nisi ut aliquip ex ea commodo consequat.")

(defvar grid-lipsum-2 "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio.

Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus.")

(grid-insert-content
 `(((:content ,grid-multiline-str :width 10 :border t :padding 1)
    (:content ,grid-lipsum-2 :width "33%" :padding 2)
    (:content ,grid-lipsum :width "33%" :padding 3))
   ((:content ,grid-multiline-str :width "49%" :border t :padding 1)
    (:content ,grid-lipsum :width "49%" :border t :padding 10))))

(grid-insert-content
 `(((:content ,grid-multiline-str :width 10 :border t :padding 2))))

(grid-insert-row
 `((:content ,grid-multiline-str :width "49%" :border t :padding 1)
   (:content ,grid-lipsum :width "49%" :border t :padding 10)))
