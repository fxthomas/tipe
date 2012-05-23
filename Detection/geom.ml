class point x y z = object (self)
  val fPosX = (x : float)
  val fPosY = (y : float)
  val fPosZ = (z : float)
  method getPosition = (fPosX,fPosY,fPosZ)
  method toString = "("^(string_of_float fPosX)^" ; "^(string_of_float fPosY)^" ; "^(string_of_float fPosZ)^")"
  method translate (v : vector) = let a,b,c = v#getDirection in new point (x +. a) (y +. b) (z +. c)
end
and vector a b c = object (self)
  val fDirX = (a : float)
  val fDirY = (b : float)
  val fDirZ = (c : float)
  method getDirection = (fDirX,fDirY,fDirZ)
  method getDirX = fDirX
  method getDirY = fDirY
  method getDirZ = fDirZ
  method toString = "["^(string_of_float fDirX)^" ; "^(string_of_float fDirY)^" ; "^(string_of_float fDirZ)^"]"
  method scalarMult (lambda : float) = new vector (fDirX *. lambda) (fDirY *. lambda) (fDirZ *. lambda)
  method add (v : vector) = new vector (fDirX+.v#getDirX) (fDirY+.v#getDirY) (fDirZ+.v#getDirZ)
  method crossProd (v : vector) = new vector (fDirY *. v#getDirZ -. fDirZ *. v#getDirY) (fDirZ *. v#getDirX -. fDirX *. v#getDirZ) (fDirX *. v#getDirY -. fDirY *. v#getDirX)
  method dotProd (v : vector) = fDirX*.v#getDirX +. fDirY*.v#getDirY +. fDirZ*.v#getDirZ
  method normalize = let s = sqrt (fDirX*.fDirX +. fDirY*.fDirY +. fDirZ*.fDirZ) in new vector (fDirX/.s) (fDirY/.s) (fDirZ/.s)
  method sqnorm = fDirX*.fDirX +. fDirY*.fDirY +. fDirZ*.fDirZ
  method rotate (q : quaternion) = (q#mult ((new quaternion 0. (new vector fDirX fDirY fDirZ))#mult (q#conjugate)))#getVector
end
and quaternion w u = object (self)
  val fReal = (w : float)
  val fVec  = (u : vector)
  method getVector = fVec
  method getReal = fReal
  method toString = "("^(string_of_float fReal)^" ; "^(fVec#toString)^")"
  method conjugate = new quaternion fReal (fVec#scalarMult (-1.))
  method sqnorm = fReal*.fReal +. fVec#sqnorm
  method scalarMult (l : float) = new quaternion (fReal *. l) (fVec#scalarMult l)
  method normalize = self#scalarMult (1. /. self#sqnorm)
  method invert = (self#conjugate)#scalarMult (1. /. self#sqnorm)
  method add (q : quaternion) = new quaternion (fReal +. q#getReal) (fVec#add (q#getVector))
  method mult (q : quaternion) = let ww = q#getReal and vv = q#getVector in let dot = fVec#dotProd vv and cross = fVec#crossProd vv in new quaternion (w*.ww -. dot) ((vv#scalarMult fReal)#add ((fVec#scalarMult ww)#add cross))
end
and line orig dir = object (self)
  val vDir = (dir : vector)
  val pOrigin = (orig : point)
  method getDirection = vDir
  method getOrigin = pOrigin
  method toString = "{ "^(pOrigin#toString)^" -> "^(vDir#toString)^" }"
  method intersectPlane (p : plane) = let x0,y0,z0 = pOrigin#getPosition and a,b,c = vDir#getDirection and u,v,w = p#getNormal#getDirection and xx,yy,zz = p#getOrigin#getPosition in
  let d = ~-. (u*.xx +. v*.yy +. w*.zz) in
  let t = ~-. (d +. x0*.u +. y0*.v +. z0*.w)/.(a*.u+.b*.v+.c*.w) in
  new point (a*.t +. x0) (b*.t +. y0) (c*.t +. z0)
end

and plane orig normal = object (self)
  val mutable vNormal = (normal : vector)
  val mutable pOrigin = (orig : point)
  method getNormal = vNormal
  method getOrigin = pOrigin
  method toString = let a,b,c = vNormal#getDirection and x,y,z = pOrigin#getPosition in
                    let d = ~-. (a*.x +. b*.y +. c*.z) in
                    (string_of_float a)^"x + "^(string_of_float b)^"y + "^(string_of_float c)^"z + "^(string_of_float d)
  method intersectPlane (p : plane) = let nd = (vNormal#crossProd (p#getNormal))#normalize and a1,b1,c1 = vNormal#getDirection and x1,y1,z1 = pOrigin#getPosition and a2,b2,c2 = p#getNormal#getDirection and x2,y2,z2 = p#getOrigin#getPosition in
  let d1 = ~-. (a1*.x1 +. b1*.y1 +.c1*.z1) and d2 = ~-. (a2*.x2 +. b2*.y2 +. c2*.z2) in
  let z = ~-. ((d2 -. b2/.b1*.d1)/.(c2 -. b2/.b1*.c1)) in let y = ~-. ((d1 -. c1*.z)/.b1) in let no = new point 0. y z in
  (new line no nd)
end;;

let rotationQuaternion phi (u : vector) =
  new quaternion (cos (phi /. 2.)) (u#scalarMult (sin (phi /. 2.)))
;;
