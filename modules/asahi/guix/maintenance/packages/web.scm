(define-module (asahi guix maintenance packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public nginx-auth-jwt-module
  (package
    (name "nginx-auth-jwt-module")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kjdev/nginx-auth-jwt.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i7mwb3bnzws666qggvlabw0fk917kvriyawixfqs4kgfrpqhsj5"))))
    (build-system gnu-build-system)
    (inputs `(("libxcrypt" ,libxcrypt)
              ("jansson" ,jansson)
              ("openssl" ,openssl)
              ("pcre" ,pcre)
              ("nginx-sources" ,(package-source nginx))
              ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no test target
       #:make-flags (list "modules")
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 popen)
                  (ice-9 regex)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-nginx-sources
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (begin
               ;; The nginx source code is part of the module’s source.
               (format #t "decompressing nginx source code~%")
               (let ((tar (assoc-ref inputs "tar"))
                     (nginx-srcs (assoc-ref inputs "nginx-sources")))
                 (invoke (string-append tar "/bin/tar")
                         "xvf" nginx-srcs "--strip-components=1"))
               #t)))
         (add-before 'configure 'patch-/bin/sh
           (lambda _
             (substitute* "auto/feature"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           ;; This phase is largely copied from the nginx package.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((flags
                    (list ;; A copy of nginx’ flags follows, otherwise we
                     ;; get a binary compatibility error.  FIXME: Code
                     ;; duplication is bad.
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--with-http_ssl_module"
                     "--with-http_v2_module"
                     "--with-pcre-jit"
                     "--with-debug"
                     "--with-compat"
                     ;; Even when not cross-building, we pass the
                     ;; --crossbuild option to avoid customizing for the
                     ;; kernel version on the build machine.
                     ,(let ((system "Linux")    ; uname -s
                            (release "3.2.0")   ; uname -r
                            ;; uname -m
                            (machine (match (or (%current-target-system)
                                                (%current-system))
                                       ("x86_64-linux"   "x86_64")
                                       ("i686-linux"     "i686")
                                       ("mips64el-linux" "mips64")
                                       ;; Prevent errors when querying
                                       ;; this package on unsupported
                                       ;; platforms, e.g. when running
                                       ;; "guix package --search="
                                       (_                "UNSUPPORTED"))))
                        (string-append "--crossbuild="
                                       system ":" release ":" machine))
                     ;; The following are the args decribed on
                     ;; <https://www.nginx.com/blog/compiling-dynamic-modules-nginx-plus>.
                     ;; Enabling --with-compat here and in the nginx package
                     ;; would ensure binary compatibility even when using
                     ;; different configure options from the main nginx
                     ;; package.  This is not needed for Guix.
                     ;; "--with-compat"
                     "--add-dynamic-module=.")))
               (setenv "CC" "gcc")
               (format #t "environment variable `CC' set to `gcc'~%")
               (format #t "configure flags: ~s~%" flags)
               (apply invoke "./configure" flags)
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (modules-dir (string-append out "/etc/nginx/modules"))
                    (doc-dir (string-append
                              out "/share/doc/nginx-auth-jwt-module")))
               (mkdir-p modules-dir)
               (copy-file "objs/ngx_http_auth_jwt_module.so"
                          (string-append
                           modules-dir "/ngx_http_auth_jwt_module.so"))
               (mkdir-p doc-dir)
               (copy-file "README.md"
                          (string-append doc-dir "/README.md"))
               #t))))))
    (home-page
     "https://github.com/kjdev/nginx-auth-jwt")
    (synopsis "Nginx module for @acronym{JWT, JSON Web Token} authentication")
    (description
     "This nginx module parses the Authentication field in HTTP headers and
does @acronym{JWT, JSON Web Token} authentication.")
    (license (delete-duplicates
              (cons license:expat ;license of nginx-auth-jwt-module
                    ;; The module’s code is linked statically with nginx,
                    ;; therefore nginx’ other licenses may also apply to its
                    ;; binary:
                    (package-license nginx))))))
