package experimental.chimneylike.dsl

import experimental.chimneylike.internal.{TransformerFlag, EnableFlag, DisableFlag}

trait FlagsDsl[UpdateFlags[_ <: Tuple], Flags <: Tuple]:
  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    */
  def enableMethodAccessors: UpdateFlags[EnableFlag[Flags, TransformerFlag.MethodAccessors]] =
    enableFlag[TransformerFlag.MethodAccessors]

  /** Disable method accessors lookup that was previously enabled by `enableMethodAccessors`
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    */
  def disableMethodAccessors: UpdateFlags[DisableFlag[Flags, TransformerFlag.MethodAccessors]] =
    disableFlag[TransformerFlag.MethodAccessors]

  /** Enable fallback to default case class values in `To` type.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def enableDefaultValues: UpdateFlags[EnableFlag[Flags, TransformerFlag.DefaultValues]] =
    enableFlag[TransformerFlag.DefaultValues]

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def disableDefaultValues: UpdateFlags[DisableFlag[Flags, TransformerFlag.DefaultValues]] =
    disableFlag[TransformerFlag.DefaultValues]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    */
  def enableBeanGetters: UpdateFlags[EnableFlag[Flags, TransformerFlag.BeanGetters]] =
    enableFlag[TransformerFlag.BeanGetters]

  /** Disable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    */
  def disableBeanGetters: UpdateFlags[DisableFlag[Flags, TransformerFlag.BeanGetters]] =
    disableFlag[TransformerFlag.BeanGetters]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    */
  def enableBeanSetters: UpdateFlags[EnableFlag[Flags, TransformerFlag.BeanSetters]] =
    enableFlag[TransformerFlag.BeanSetters]

  /** Disable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    */
  def disableBeanSetters: UpdateFlags[DisableFlag[Flags, TransformerFlag.BeanSetters]] =
    disableFlag[TransformerFlag.BeanSetters]

  /** Sets target value of optional field to None if field is missing from source type `From`.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    */
  def enableOptionDefaultsToNone: UpdateFlags[EnableFlag[Flags, TransformerFlag.OptionDefaultsToNone]] =
    enableFlag[TransformerFlag.OptionDefaultsToNone]

  /** Disable `None` fallback value for optional fields in `To`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    */
  def disableOptionDefaultsToNone: UpdateFlags[DisableFlag[Flags, TransformerFlag.OptionDefaultsToNone]] =
    disableFlag[TransformerFlag.OptionDefaultsToNone]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def enableUnsafeOption: UpdateFlags[EnableFlag[Flags, TransformerFlag.UnsafeOption]] =
    enableFlag[TransformerFlag.UnsafeOption]

  /** Disable unsafe value extraction from optional fields in `From` type.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    */
  def disableUnsafeOption: UpdateFlags[DisableFlag[Flags, TransformerFlag.UnsafeOption]] =
    disableFlag[TransformerFlag.UnsafeOption]

  private inline def enableFlag[F <: TransformerFlag]: UpdateFlags[EnableFlag[Flags, F]] =
    this.asInstanceOf[UpdateFlags[EnableFlag[Flags, F]]]

  private inline def disableFlag[F <: TransformerFlag]: UpdateFlags[DisableFlag[Flags, F]] =
    this.asInstanceOf[UpdateFlags[DisableFlag[Flags, F]]]

end FlagsDsl