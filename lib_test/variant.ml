type t = 
  |Foo
  |Bar of int
  |Xyz of string

and
x = {
  foo: t;
  bar: int;
}
with persist()


