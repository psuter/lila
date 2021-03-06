package lila.setup

import chess.Variant
import lila.rating.RatingRange
import lila.db.api._
import lila.lobby.Color
import lila.user.UserContext
import play.api.data._
import play.api.data.Forms._
import tube.{ userConfigTube, anonConfigTube }

private[setup] final class FormFactory(casualOnly: Boolean) {

  import Mappings._

  def filterFilled(implicit ctx: UserContext): Fu[(Form[FilterConfig], FilterConfig)] =
    filterConfig map { f => filter(ctx).fill(f) -> f }

  def filter(ctx: UserContext) = Form(
    mapping(
      "variant" -> list(variantWithKoth),
      "mode" -> list(rawMode(withRated = true)),
      "speed" -> list(speed),
      "ratingRange" -> ratingRange
    )(FilterConfig.<<)(_.>>)
  )

  def filterConfig(implicit ctx: UserContext): Fu[FilterConfig] = savedConfig map (_.filter)

  def aiFilled(fen: Option[String])(implicit ctx: UserContext): Fu[Form[AiConfig]] =
    aiConfig map { config =>
      ai(ctx) fill fen.fold(config) { f =>
        config.copy(fen = f.some, variant = Variant.FromPosition)
      }
    }

  def ai(ctx: UserContext) = Form(
    mapping(
      "variant" -> variantWithFen,
      "clock" -> boolean,
      "time" -> time,
      "increment" -> increment,
      "level" -> level,
      "color" -> color,
      "fen" -> fen
    )(AiConfig.<<)(_.>>)
      .verifying("Invalid FEN", _.validFen)
  )

  def aiConfig(implicit ctx: UserContext): Fu[AiConfig] = savedConfig map (_.ai)

  def friendFilled(fen: Option[String])(implicit ctx: UserContext): Fu[Form[FriendConfig]] =
    friendConfig map { config =>
      friend(ctx) fill fen.fold(config) { f =>
        config.copy(fen = f.some, variant = Variant.FromPosition)
      }
    }

  def friend(ctx: UserContext) = Form(
    mapping(
      "variant" -> variantWithFenAndKoth,
      "clock" -> boolean,
      "time" -> time,
      "increment" -> increment,
      "mode" -> mode(withRated = ctx.isAuth && !casualOnly),
      "color" -> color,
      "fen" -> fen
    )(FriendConfig.<<)(_.>>)
      .verifying("Invalid clock", _.validClock)
      .verifying("Invalid FEN", _.validFen)
  )

  def friendConfig(implicit ctx: UserContext): Fu[FriendConfig] = savedConfig map (_.friend)

  def hookFilled(implicit ctx: UserContext): Fu[Form[HookConfig]] =
    hookConfig map hook(ctx).fill

  def hook(ctx: UserContext) = Form(
    mapping(
      "variant" -> variantWithKoth,
      "clock" -> boolean,
      "time" -> time,
      "increment" -> increment,
      "mode" -> mode(ctx.isAuth && !casualOnly),
      "membersOnly" -> boolean,
      "ratingRange" -> optional(ratingRange),
      "color" -> nonEmptyText.verifying(Color.names contains _)
    )(HookConfig.<<)(_.>>)
      .verifying("Invalid clock", _.validClock)
      .verifying("Can't create rated unlimited in lobby", _.noRatedUnlimited)
  )

  def hookConfig(implicit ctx: UserContext): Fu[HookConfig] = savedConfig map (_.hook)

  def savedConfig(implicit ctx: UserContext): Fu[UserConfig] =
    ctx.me.fold(AnonConfigRepo config ctx.req)(UserConfigRepo.config)
}
