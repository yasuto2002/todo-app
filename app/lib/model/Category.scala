package lib.model

import ixias.model.{@@, Entity, EntityModel, Identity, NOW}
import shapeless.{the}
import Category._
import ixias.util.EnumStatus

import java.time.LocalDateTime
case class Category(
  id:         Option[Id],
  name :     String,
  slug :     String,
  color:      Color,
  updatedAt:  LocalDateTime = NOW,
  createdAt: LocalDateTime = NOW
)extends EntityModel[Id]

object Category{

  val   Id = the[Identity[Id]]
  type  Id = Long @@ Category
  type WithNoId = Entity.WithNoId[Id,Category]
  type EmbeddedId = Entity.EmbeddedId[Id,Category]

  sealed abstract class Color(val code:Short,val name:String,val rgb:String) extends EnumStatus

  object Color extends EnumStatus.Of[Color]{
    case object RED extends Color(code = 1,name = "赤",rgb = "#ff0000" )
    case object BLUE extends Color(code = 2,name = "青",rgb = "#0000ff")
    case object GREEN extends Color(code = 3,name = "緑",rgb = "#00ff00")
  }
  def apply(name:String,slug:String,color:Color):WithNoId = {
    new Entity.WithNoId(
      new Category(
        id    = None,
        name  = name,
        slug  = slug,
        color = color
      )
    )
  }

  def unapply(category:Category#WithNoId):Option[(String,String,Color)] = {
    Some((category.v.name,category.v.slug,category.v.color))
  }

  def build(category: Category):Category#EmbeddedId ={
    new Entity.EmbeddedId(category)
  }
}