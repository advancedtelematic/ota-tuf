package com.advancedtelematic.ota_tuf.daemon

import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, Props, Status, SupervisorStrategy}
import akka.routing.RoundRobinPool
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.daemon.KeyGeneratorLeader.Tick
import com.advancedtelematic.ota_tuf.data.DataType.{Key, KeyGenRequest, Role, RoleId}
import com.advancedtelematic.ota_tuf.data.{KeyGenRequestStatus, KeyType}
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import slick.driver.MySQLDriver.api._

import scala.async.Async._
import scala.concurrent.duration._
import akka.pattern.pipe
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}


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


class KeyGenerationOp(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with RoleRepositorySupport {

  import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair._

  private lazy val _log = LoggerFactory.getLogger(this.getClass)

  def processGenerationRequest(kgr: KeyGenRequest): Future[Key] =
    async {
      val role = Role(RoleId.generate(), kgr.groupId, kgr.roleType, kgr.threshold)
      await(roleRepo.persist(role))

      val keyPair = RsaKeyPair.generate(kgr.keySize)
      val key = Key(keyPair.id, role.id, KeyType.RSA, keyPair.getPublic)

      val vaultKey = VaultKey(key.id, key.keyType, key.publicKey.show, keyPair.getPrivate.show)
      await(vaultClient.createKey(vaultKey))

      await(keyGenRepo.persistGenerated(kgr, key, keyRepo))

      _log.info("Generated Key {}", key.id.get)
      key
    }
}

object KeyGeneratorWorker {
  def props(vaultClient: VaultClient)(implicit db: Database): Props = Props(new KeyGeneratorWorker(vaultClient))
}

class KeyGeneratorWorker(vaultClient: VaultClient)(implicit val db: Database) extends Actor
  with ActorLogging
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with RoleRepositorySupport {

  implicit val ec = context.dispatcher

  val keyGenRequestOp = new KeyGenerationOp(vaultClient)

  override def receive: Receive = {
    case kgr: KeyGenRequest =>
      log.info(s"Received key gen request for {}", kgr.id.show)

      keyGenRequestOp.processGenerationRequest(kgr)
        .map(Success)
        .recoverWith {
          case ex ⇒
            log.error("Key generation failed: {}", ex.getMessage)
            keyGenRepo
              .setStatus(kgr.id, KeyGenRequestStatus.ERROR)
              .map(_ => Failure(ex))
        }.pipeTo(sender)
  }
}
