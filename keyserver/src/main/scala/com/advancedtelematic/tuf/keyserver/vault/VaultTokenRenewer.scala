package com.advancedtelematic.tuf.keyserver.vault

import akka.Done
import akka.actor.{Actor, ActorLogging, Props, Status}
import com.advancedtelematic.tuf.keyserver.vault.VaultTokenRenewer.{Renew, Renewed}
import scala.concurrent.duration._
import akka.pattern.{Backoff, BackoffSupervisor, pipe}

object VaultTokenRenewer {
  case object Renew
  case object Renewed

  protected def props(vaultClient: VaultClient, renewInterval: FiniteDuration): Props =
    Props(new VaultTokenRenewer(vaultClient, renewInterval))

  def withBackoff(vaultClient: VaultClient, renewInterval: FiniteDuration): Props =
    BackoffSupervisor.props(Backoff.onFailure(
      props(vaultClient, renewInterval),
      childName = "VaultTokenRenewer",
      minBackoff = 10.seconds,
      maxBackoff = 5.minutes,
      randomFactor = 0.2)
    )
}

class VaultTokenRenewer(vaultClient: VaultClient, renewInterval: FiniteDuration)
  extends Actor with ActorLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    self ! Renew
  }

  override def receive: Receive = {
    case Renew =>
      log.info("Renewing vault token")
      vaultClient
        .renewToken()
        .map(_ => Done).pipeTo(self)
    case Status.Failure(ex) =>
      log.error(s"Could not renew vault token: ${ex.getMessage}")
      throw ex
    case Done =>
      log.info(s"Renewed vault token for $renewInterval, renewing again in $renewInterval")
      context.parent ! Renewed
      context.system.scheduler.scheduleOnce(renewInterval, self, Renew)
  }
}
