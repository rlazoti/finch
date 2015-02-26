package io.finch

import com.twitter.finagle.Service
import com.twitter.util.Future

import io.finch.route.{Endpoint => _, _}
import io.finch.response._
import io.finch.request._

/**
 * An experimental package that enables `micro`-services support in Finch.
 */
package object micro {

  /**
   * `RequestReader` is a composable microservice or just `Micro`.
   */
  type Micro[A] = RequestReader[A]

  /**
   *
   */
  type HttpMicro = Micro[HttpResponse]

  /**
   * A companion object for `Micro`.
   */
  val Micro = RequestReader

  /**
   * A `Router` that extract `Micro` is called an `Endpoint`.
   */
  type Endpoint[A] = Router[Micro[A]]

  /**
   *
   */
  type HttpEndpoint = Endpoint[HttpResponse]

  /**
   * An implicit class that enables a _forced or_ compositor for routers.
   */
  implicit class EndpointForcedOr[A](val self: Endpoint[A])(implicit evA: A => HttpResponse) {
    def ![B](that: Endpoint[B])(implicit evB: B => HttpResponse): HttpEndpoint =
      self.map(_.map(evA)) | that.map(_.map(evB))
  }

  /**
   * Enables an implicit view `A => HttpResponse` if there is a corresponding type class `EncodeResponse` available for
   * `A`.
   */
  implicit def anyToHttp[A](a: A)(implicit e: EncodeResponse[A]): HttpResponse = Ok(a)

  /**
   * Converts any `Micro[A]` to ???.
   */
  implicit def anyMicroToHttpMicro[A](
    m: Micro[A]
  )(implicit ev: A => HttpResponse): Micro[HttpResponse] = m.map(ev)

  implicit def anyMicroToHttpService[A](
    m: Micro[A]
  )(implicit ev: A => HttpResponse): Service[HttpRequest, HttpResponse] = new Service[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest): Future[HttpResponse] = m.map(ev)(req)
  }

  implicit def anyRouterToHttpRouter[A](
    r: RouterN[A]
  )(implicit ev: A => Micro[HttpResponse]): RouterN[Micro[HttpResponse]] = r.map(ev)

  /**
   * Implicitly converts new-style endpoint into `Service`.
   */
  implicit def anyRouterToHttpService[A](
   e: RouterN[A]
  )(implicit ev: A => Micro[HttpResponse]): Service[HttpRequest, HttpResponse] =
    new Service[HttpRequest, HttpResponse] {
      def apply(req: HttpRequest): Future[HttpResponse] = e.map(ev)(requestToRoute(req)) match {
        case Some((Nil, rr)) => rr(req)
        case _ => RouteNotFound(s"${req.method.toString.toUpperCase} ${req.path}").toFutureException
      }
    }
}
