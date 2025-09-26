;; Yes, it's backwards
format : [byte](x : int) = {
  out : [byte];

  while x, {
    out += byte 48 + x % 10;
    x /= 10;
  };

  out += byte 0;

  out;
};

print 96;
print "wow!!"[0];
