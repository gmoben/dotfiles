(require 'rotate)

;; Reverse v/h on purpose to reflect the orientation of the frames
;; rather than the window
(global-set-key (kbd "C-c r v") 'rotate:even-horizontal)
(global-set-key (kbd "C-c r h") 'rotate:even-vertical)
(global-set-key (kbd "C-c r t") 'rotate:tiled)
(global-set-key (kbd "C-c r w") 'rotate-window)
(global-set-key (kbd "C-c r l") 'rotate-layout)
(global-set-key (kbd "C-c r m v") 'rotate:main-horizontal)
(global-set-key (kbd "C-c r m h") 'rotate:main-vertical)
