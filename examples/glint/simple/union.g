foo : union {
    ;; TODO: Disallow initializers (if not already)
    x :cint;
    y :uint;
};

;; TODO: Disallow initializer
bar :foo;
bar.y := 69;
bar.x := 32;

bar.x;
