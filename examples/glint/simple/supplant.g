foo : struct {
    x : int;
};

bar : struct {
    supplant foo;
};

baz : bar;
baz.x := 69;

baz.x;
