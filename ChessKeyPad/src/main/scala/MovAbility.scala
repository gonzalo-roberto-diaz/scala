/**
  * The ability of a game piece to move in a specific set of directions.
  * It has several convenience parameters in order to save typing long list of coordinates that are the result of some
  * common geometrical transformation or each other
  */
case class MovAbility(vector: Point, repeatable: Boolean = false, coordFlip: Boolean = false, mirrorX: Boolean = false, mirrorY: Boolean = false, centralSym: Boolean = false)
