Creates thunk with custom equality

Also a unique reference is created, so that one application of rendering function (`f` argument) and equality (`eqFn` argument) is only ever equal to itself

It's done because of a distinction of how compiler generalizes equality functions
For example:
1. something like `Eq String` would be OK, since it always returns the same dictionary, and thus the same eq function.

```js
var eqString = new Eq($foreign.eqStringImpl);
```

2. something like `Eq (Array String)` probably would not be OK, since it creates a new dictionary on the fly parameterized by String,

```js
var eqArray = function (dictEq) {
  return new Eq($foreign.eqArrayImpl(eq(dictEq)));
};
```


Unique reference forces you to provide equality function beforehand even when you use something like `Eq String`

e.g.

1. This is wrong, `is_eq` will return `false`

```purs
thunkCreator :: forall a . Eq a => a -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = thunkCreator "a"
bar = thunkCreator "a"
is_eq = Fn.runFn2 unsafeEqThunk foo bar
```

because it is compiled to

```js
var thunkCreator = function (dictEq) {
    return thunked(Data_Eq.eq(dictEq))(function (a) {
        return [ 42 ];
    });
};
var foo = thunkCreator(Data_Eq.eqString)("a");
var bar = thunkCreator(Data_Eq.eqString)("a");
var is_eq = unsafeEqThunk(foo, bar);
```

2. This is right, `is_eq` will return `true`

```purs
thunkCreator :: String -> Thunk Array Int
thunkCreator = thunked eq (\a -> [42])
foo = thunkCreator "a"
bar = thunkCreator "a"
is_eq = Fn.runFn2 unsafeEqThunk foo bar
```

because it is compiled to

```js
var thunkCreator = thunked(Data_Eq.eq(Data_Eq.eqString))(function (a) {
    return [ 42 ];
});
var foo = thunkCreator("a");
var bar = thunkCreator("a");
var is_eq = unsafeEqThunk(foo, bar);
```
