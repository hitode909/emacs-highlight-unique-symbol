# highlight-unique-symbol

* Highlight symbols which not appear in the repository.
* That may be typo, miss spell or unused method.
* Support Git repositories.

## Usage

```lisp
(require 'highlight-unique-symbol)
(highlight-unique-symbol t)
```

## Configuring

You can configure these settings with `M-x customize-group RET highlight-unique-symbol RET`.

<dl>
  <dt>highlight-unique-symbol:interval</dt>
  <dd>Interval to check symbol at cursor.</dd>
  <dt>highlight-unique-symbol:face</dt>
  <dd>Face of unique symbols.</dd>
</dl>
