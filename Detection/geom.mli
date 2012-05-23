class point :
  float ->
  float ->
  float ->
  object
    val fPosX : float
    val fPosY : float
    val fPosZ : float
    method getPosition : float * float * float
    method toString : string
    method translate : vector -> point
  end
and vector :
  float ->
  float ->
  float ->
  object
    val fDirX : float
    val fDirY : float
    val fDirZ : float
    method add : vector -> vector
    method crossProd : vector -> vector
    method dotProd : vector -> float
    method getDirX : float
    method getDirY : float
    method getDirZ : float
    method getDirection : float * float * float
    method normalize : vector
    method rotate : quaternion -> vector
    method scalarMult : float -> vector
    method sqnorm : float
    method toString : string
  end
and quaternion :
  float ->
  vector ->
  object
    val fReal : float
    val fVec : vector
    method add : quaternion -> quaternion
    method conjugate : quaternion
    method getReal : float
    method getVector : vector
    method invert : quaternion
    method mult : quaternion -> quaternion
    method normalize : quaternion
    method scalarMult : float -> quaternion
    method sqnorm : float
    method toString : string
  end
and line :
  point ->
  vector ->
  object
    val pOrigin : point
    val vDir : vector
    method getDirection : vector
    method getOrigin : point
    method intersectPlane : plane -> point
    method toString : string
  end
and plane :
  point ->
  vector ->
  object
    val mutable pOrigin : point
    val mutable vNormal : vector
    method getNormal : vector
    method getOrigin : point
    method intersectPlane : plane -> line
    method toString : string
  end
val rotationQuaternion : float -> vector -> quaternion
