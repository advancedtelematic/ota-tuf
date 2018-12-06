package com.advancedtelematic.tuf.reposerver.db

import com.advancedtelematic.libats.slick.codecs.SlickEnumMapper
import com.advancedtelematic.libats.slick.db.SlickCirceMapper
import com.advancedtelematic.libtuf.data.ValidatedString
import com.advancedtelematic.libtuf.data.ValidatedString.{ValidatedString, ValidatedStringValidation}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod

import scala.reflect.ClassTag

object SlickMappings {
  implicit val storageMethodMapping = SlickEnumMapper.enumMapper(StorageMethod)
}

object SlickValidatedString {
  implicit def validatedStringMapper[W <: ValidatedString : ClassTag](implicit validation: ValidatedStringValidation[W]) = {
    implicit val decoder = ValidatedString.validatedStringDecoder[W]
    implicit val encoder = ValidatedString.validatedStringEncoder[W]
    SlickCirceMapper.circeMapper[W]
  }
}