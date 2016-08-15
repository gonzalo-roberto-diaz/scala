/**
  * The ability of a game piece to move in a specific set of directions.
  * It has several convenience parameters in order to save
  *
  */
case class MovAbility(vector: Point, repeatable: Boolean = false, coordFlip: Boolean = false, mirrorX: Boolean = false, mirrorY: Boolean = false, centralSym: Boolean = false)
