package model

case class ViewValueCategory(
  title:  String,
  cssSrc: Seq[String],
  jsSrc:  Seq[String],
) extends ViewValueCommon
