(hall-description
  (name "asahi-guix-maintenance")
  (prefix "")
  (version "0.1")
  (author "Asahi Guix")
  (email "")
  (copyright (2024))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (features
    ((guix #f)
     (use-guix-specs-for-dependencies #f)
     (native-language-support #f)
     (licensing #f)))
  (files (libraries
           ((scheme-file "asahi-guix-maintenance")
            (directory
              "asahi-guix-maintenance"
              ((scheme-file "hconfig")))))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory
              "doc"
              ((texi-file "asahi-guix-maintenance")))))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")))))
