package com.advancedtelematic.ota_tuf.daemon

import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, Props, Status, SupervisorStrategy}

import akka.routing.RoundRobinPool
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.daemon.KeyGeneratorLeader.Tick
import com.advancedtelematic.ota_tuf.data.DataType.KeyId._
import com.advancedtelematic.ota_tuf.data.DataType.{Key, KeyGenRequest}
import com.advancedtelematic.ota_tuf.data.{KeyGenRequestStatus, KeyType}
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import slick.driver.MySQLDriver.api._

import scala.async.Async._
import scala.concurrent.duration._

import akka.pattern.pipe

object KeyGeneratorLeader {
  case object Tick

  def props(vaultClient: VaultClient)(implicit db: Database): Props = Props(new KeyGeneratorLeader(vaultClient))
}

class KeyGeneratorLeader(vaultClient: VaultClient)(implicit val db: Database) extends Actor with ActorLogging with KeyGenRequestSupport {

  implicit val ec = context.dispatcher

  private val WORKER_COUNT = 10

  override def preStart(): Unit = {
    self ! Tick
  }

  // override def postRestart(reason: Throwable): Unit = ()

  private val router = {
    val routerProps = RoundRobinPool(WORKER_COUNT)
      .withSupervisorStrategy(SupervisorStrategy.defaultStrategy)
      .props(KeyGeneratorWorker.props(vaultClient))

     context.system.actorOf(routerProps)
   }

  def waiting(totalTasks: Int, remaining: Int): Receive =
    if(remaining == 0) {
      log.info("Finished generating {} keys", totalTasks)
      context.system.scheduler.scheduleOnce(3.seconds, self, Tick)
      receive
    } else {
      {
        case Status.Success(_) ⇒
          context.become(waiting(totalTasks, remaining - 1))
        case Status.Failure(ex) ⇒
          log.error(ex, "Could not generate key")
          context.become(waiting(totalTasks, remaining - 1))
      }
    }


  override def receive: Receive = {
    case Status.Failure(ex) ⇒
      throw ex

    case taskCount: Int ⇒
      log.info("Waiting for {} key generation tasks to complete", taskCount)
      context become waiting(taskCount, taskCount)

    case Tick =>
      log.info("Tick")

      val f = keyGenRepo.findPending(limit = WORKER_COUNT * 4).map { m =>
        m.foreach { router ! _ }
        m.size
      }

      f.pipeTo(self)
  }
}

object KeyGeneratorWorker {
  def props(vaultClient: VaultClient)(implicit db: Database): Props = Props(new KeyGeneratorWorker(vaultClient))
}

class KeyGeneratorWorker(vaultClient: VaultClient)(implicit val db: Database) extends Actor
  with ActorLogging
  with KeyGenRequestSupport
  with KeyRepositorySupport {

  import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair._
  implicit val ec = context.dispatcher

  override def receive: Receive = {
    case kgr: KeyGenRequest =>
      log.info(s"Received key gen request for {}", kgr.id.show)

      val f = async {
        val keyPair = RsaKeyPair.generate(kgr.size)
        val key = Key(kgr.id, KeyType.RSA, keyPair.getPublic.show)

        val newKeyGnr = await(keyGenRepo.persistGenerated(kgr, key, keyRepo))

        val vaultKey = VaultKey(kgr.id, key.keyType, key.publicKey, keyPair.getPrivate.show)
        await(vaultClient.createKey(vaultKey))

        log.info("Generated Key {}", key.id.show)
        Success(newKeyGnr)
      }

      f.recoverWith {
        case ex ⇒
          log.error("Key generation failed: {}", ex.getMessage)
          keyGenRepo
            .setStatus(kgr.id, KeyGenRequestStatus.ERROR)
            .map(_ => Failure(ex))
      }.pipeTo(sender)
  }
}
