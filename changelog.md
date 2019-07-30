# doctemplates

## 0.3.0.1

* Bump lower bound on base to 4.9, drop support for ghc 7.10.

* Add needed import for older base versions.

* Add test.hs to repository.

## 0.3

* Note that all of the changes to template syntax described
  below are backwards compatible, and all old pandoc templates
  should continue to work as before.

* Allow `${...}` style delimiters around variables and
  directives, in addition to `$...$`.  Allow space around
  the delimiters.

* Support `$it$` as a variable for the current value in
  an iteration.  (The old method, where the containing
  variable name is used, still works.)

* Support partials (subtemplates defined in different files).

* Interpolated array variables now have all elements rendered,
  concatenated, with an optional separator that can be
  specified using a new bracketed syntax.

* Remove `TemplateTarget` class.  It was pointless; the
  calling program can just do these trivial transformations.
  Avoids dependencies on bytestring, blaze-html, blaze-markup.

* Change type of `renderTemplate` and `applyTemplate` to produce
  a `Text`, instead of being polymorphic.

* Changed type of `compileTemplate`: it now takes a
  template path and the template contents, and returns
  either a template or an error.  It runs in an instance
  of `TemplateMonad`, which is an abstraction around different
  ways of getting partials.  (For example, in IO we can get
  partials by reading them from a file system, but in a
  web application one might want to obtain them from the
  database or have a set of them baked in.)

* Remove `varListToJSON`.

* Changed the architecture: `Template` is no longer just
  a newtype around a function, but a list of `TemplatePart`s.

* Added a newtype for `Variable`.

* Improved documentation in README.md.

* Added a new test framework and much more extensive tests.

