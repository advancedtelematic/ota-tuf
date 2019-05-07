package com.advancedtelematic.tuf.reposerver.db

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import org.slf4j.LoggerFactory
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
import SlickMappings._
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{StorageMethod, TargetItem}

import scala.concurrent.{ExecutionContext, Future}

class TreehubStorageMethodFix(implicit
                              val db: Database,
                              val mat: Materializer,
                              val system: ActorSystem,
                              val ec: ExecutionContext
                             ) {

  import Schema.targetItems

  private val _log = LoggerFactory.getLogger(this.getClass)

  def needsFix(item: TargetItem): Boolean =
    item.uri.toString.contains("https://treehub.atsgarage.com/api/v2/mydevice") && item.storageMethod == StorageMethod.Managed

  def fixTargetItemIfNeeded = Flow[TargetItem].mapAsync(3) { item =>
    if(needsFix(item))
      db.run(targetItems.filter(_.repoId === item.repoId).filter(_.filename === item.filename).map(_.storageMethod).update(StorageMethod.Unmanaged))
        .map { _ =>
          _log.info(s"Processed item (${item.repoId}, ${item.filename})")
        }
    else
      FastFuture.successful(item)
  }

  def run: Future[Done] = {
    val dbStream = db.stream(targetItems.filter(_.storageMethod === StorageMethod.Managed).result)
    Source.fromPublisher(dbStream).via(fixTargetItemIfNeeded).runWith(Sink.ignore)
  }
}
