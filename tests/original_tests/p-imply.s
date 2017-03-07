main { 
  a:bool := T;
  b:bool := F;
  c:bool := a => b;
  print c;

  c:bool := a = null;
};