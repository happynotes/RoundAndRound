#' Class of planet
#' \code{SpaceObject}
#' @slot shape Ploting function of the shape
#' @slot radius Radius for sphere
#' @slot Period.Rotate data.frame 1*3 c(Period.Rotate, Period.Orbit, Period.Synodic)
#' @importFrom methods new
#' @return Class of SpaceObject
#' @export
SpaceObject <- methods::setClass("SpaceObject",
                                slots = c(shape ='function',
                                          radius="numeric",
                                          Period.Rotate ="data.frame"
                                          ) )


#' Class of Orbit
#' \code{Orbit}
#' @slot ab Shape of the object, ab=1 Sphere, ab!=1 Ellipsoid
#' @slot e eccentric of the orbit
#' @slot radius Radius for sphere (ab=1), or Semi-major axis for Ellipsoid (ab!=1)
#' @slot period data.frame 1*3 c(Period.Rotate, Period.Orbit, Period.Synodic)
#' @slot Inclination Inclination.
#' @slot CenterObject Central Object.
#' @importFrom methods new
#' @return Class of SpaceOrbit
#' @export
SpaceOrbit <- methods::setClass("SpaceOrbit",
                                 slots = c(ab='numeric',
                                           e = 'numeric',
                                           radius="numeric",
                                           period ="data.frame",
                                           Inclination = 'numeric',
                                           CenterObject ='character'
                                 ) )
#' This is data to be included in my package
#' @name FactSheet
#' @docType data
#' @keywords data
NULL

