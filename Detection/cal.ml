(* Camera Abstraction Layer Module : Converts units from pixels to view angles *)
(*  ~Units // Length : m *)

(*
#load "geom.cmo";;
*)
open Geom;;

class camera = object (self)
  val mutable pPos          = new point 0. 0. 0. (* caméra de base à l'origine *)
  val mutable vLookAt       = new vector 0. 0. 1. (* regarde en avant sur l'axe z *)
  val mutable fTimeInterval = ( 1./.30. : float ) (* 30fps *)
  val mutable fSensorH      = ( 0.00431 : float ) (* sony dsc-h10 : capteur de 5.75x4.31 mm *)
  val mutable fSensorW      = ( 0.00575 : float )
  val mutable fFocalLength  = ( 0.00630 : float )
  method getPosition = pPos
  method setPosition p = pPos <- p
  method getLookAt = vLookAt
  method setLookAt (vLa : vector) = vLookAt <- vLa#normalize
  method getTimeInterval = fTimeInterval
  method setTimeInterval dt = fTimeInterval <- dt
  method setSensorSize h w = fSensorH <- h; fSensorW <- w
  method getSensorSize = (fSensorH,fSensorW)
  method setFocalLength f = fFocalLength <- f
  method getFocalLength = fFocalLength
  method getBaseVectors = let a,_,c = vLookAt#getDirection in let z = vLookAt in let e = sqrt (c*.c /. (c*.c +. a*.a)) in let x = new vector e 0. (~-. a/.c*.e) in let y = z#crossProd x in (x,y,z)
  method getLookAtAngle = let z = new vector 0. 0. 1. in (if vLookAt#getDirY >= 0. then -1. else 1.) *. asin (sqrt (((vLookAt#crossProd z)#scalarMult (vLookAt#dotProd z))#sqnorm))
  method getRotationQuaternion = let phi = self#getLookAtAngle and (x1,_,_) = self#getBaseVectors in let q1 = rotationQuaternion phi x1 in q1
end;;

let z = new quaternion 
