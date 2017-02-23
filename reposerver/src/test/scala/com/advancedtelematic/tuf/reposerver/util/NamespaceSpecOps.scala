package com.advancedtelematic.tuf.reposerver.util

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.headers.RawHeader

object NamespaceSpecOps {
  trait NamespaceTag {
    val value: String
  }

  def withNamespace[T](ns: String)(fn: NamespaceTag => T): T = {
    fn.apply(new NamespaceTag {
      override val value = ns
    })
  }

  implicit class Namespaced(value: HttpRequest) {
    def namespaced(implicit namespaceTag: NamespaceTag): HttpRequest =
      value.addHeader(RawHeader("x-ats-namespace", namespaceTag.value))
  }
}
