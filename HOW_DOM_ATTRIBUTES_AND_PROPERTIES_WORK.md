# How .attributes work

`$0.attributes` are:
- <input required> is { required: "" }
- <input required="false"> is { required: "false" }
- <td colspan="1"> is { colspan: "1" }, but prop is colSpan = 1
- <div foo:data-foo="1"> is { "foo:data-foo": "1" }

# Properties

```
Having `<input type="text" value="foo1">`
If do `$0.required = true` in chrome.
  THE html `<input type="text" value="foo1" required>`
  THE $0.attributes = { required: "true" }
```

```
Having `<input type="text" value="foo1" required>`
If do `$0.required = false` in chrome.
  THE html `<input type="text" value="foo1">`
  THE $0.attributes = {}
```

# How .dataset property works? should we support it?

```
Having `<div id="1" data-foo="foo" data-bar="bar" data-baz-bak="baz-bak" data-someint="1"></div>`
  The `$0.attributes`
    `NamedNodeMap { id: "1", data-foo: "foo", data-bar: "bar", data-baz-bak: "baz-bak", someint: "1" }`
  The `$0.dataset`
    `DOMStringMap { foo: "foo", bar: "bar", bazBak: "baz-bak", someint: "1" }`
```

Also, react doesnt support dataset property during hydration. Proof:

- server = `<div id="1" data-foo="foo">`
- client = `<div id="1" dataset={{ foo : "foo" }}></div>`
- errorMessage = `Prop `dataset` did not match. Server: "null" Client: "[object Object]"`

thus, react does support `data-***` attributes, but doesn't support `dataset` property, so why bother with supporting `dataset` property?
