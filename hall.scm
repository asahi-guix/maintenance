(hall-description
  (name "maintenance")
  (prefix "")
  (version "0.0.1")
  (author "Asahi Guix")
  (email "")
  (copyright (2024))
  (synopsis "Asahi Guix Maintenance")
  (description "Asahi Guix Maintenance")
  (home-page
    "https://github.com/asahi-guix/maintenance")
  (license gpl3+)
  (dependencies `(("guile-hall" ,guile-hall)))
  (skip ())
  (features
    ((guix #f)
     (use-guix-specs-for-dependencies #f)
     (native-language-support #f)
     (licensing #f)))
  (files (libraries
           ((directory
              "asahi"
              ((directory
                 "guix"
                 ((directory
                    "maintenance"
                    ((directory "packages" ((scheme-file "ci")))
                     (directory "systems" ((scheme-file "server")))
                     (directory "machines" ((scheme-file "server")))
                     (scheme-file "hconfig")))))))))
         (tests ((directory "tests" ())))
         (programs
           ((directory "scripts" ((text-file "deploy")))))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory
              "doc"
              ((texi-file "version")
               (text-file "stamp-vti")
               (text-file ".dirstamp")
               (org-file "NOTES")
               (texi-file "asahi-guix-maintenance")
               (info-file "asahi-guix-maintenance")))))
         (infrastructure
           ((directory
              ".guix"
              ((directory
                 "modules"
                 ((scheme-file "guile-package")))))
            (text-file ".gitignore")
            (scheme-file "hall")
            (symlink
              "guix.scm"
              ".guix/modules/guile-package.scm")
            (directory
              ".github"
              ((directory
                 "workflows"
                 ((unknown-type "test.yml")))))
            (directory
              "share"
              ((directory
                 "files"
                 ((directory
                    "ssh"
                    ((directory
                       "authorized-keys"
                       ((directory
                          "rsa"
                          ((unknown-type "roman.pub")
                           (unknown-type "root.pub")))
                        (directory
                          "ed25519"
                          ((unknown-type "roman.pub")))))))
                  (directory
                    "authorized-keys"
                    ((unknown-type "apple-m1.pub")))))
               (directory
                 "patches"
                 ((unknown-type "cuirass-disable-jit.patch")))))))))
