# doctemplates

## 0.5

  * Add toText method to TemplateTarget class.

  * Add String and Lazy Text instances for TemplateTarget.

  * Swap Parameters in ToContext (so that the first parameter
    for both ToContext and FromContext refers to the parameter
    of Context).

  * Add toVal method to ToContext.

  * Default instance definition for toContext in terms of toVal,
    so that defining toVal is sufficient.

  * Add instances for ToContext and FromContext.

  * Remove valueToContext. Add ToJSON, FromJSON instances
    for Context and Val instead.

  * isEmpty: For Doc, treat `Text 0 _` as empty.
    Also `Concat x y` when x and y are empty.
    This differs from isEmpty in DocLayout itself, which only
    applies to Empty.

  * Code cleanup.

## 0.4

  * Split into three modules.  Main module only exports an
    opaque version of the Template type.  Import Internal if you
    need to manipulate a Template.

  * Add Context type, parameterized on the underlying content's type.

  * Add Val type.

  * Add valueToContext for converting an Aeson Value to a Context.

  * Make renderTemplate and applyTemplate polymorphic in both
    context and target.  Context parameter is now any instance
    of ToContext (instead of ToJSON).  Result is now any
    instance of TemplateTarget.

  * Change type of getPartial in TemplateMonad so it runs in the
    TemplateMonad instance, not the Parser.  Return a simple
    value rather than an Either; error handling can vary with
    the monad.

  * Remove TemplatePart. Template is now an algebraci data type,
    not a list of TemplateParts.

  * Add an Indented type to indicate indentation for
    interpolated variables.

  * Improve architecture, doing more at compile time.

  * Depend on doclayout.  Context can be parameterized on a doclayout
    Doc type, allowing intelligent reflowing of content.

  * Remove single final newline in interpolated variable.

  * Remove final newline from partial.

  * Don't iterate when the variable evaluates to NullVal.

  * Only indented interpolated variables if by themselves on line.

  * Add Indented parameter to Interpolate constructor.

  * Update documentation and haddocks.

  * Add benchmark.

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

