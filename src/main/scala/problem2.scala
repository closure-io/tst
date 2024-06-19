case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
  val sortedCombos = (1 until (1 << allPromotions.length)).flatMap { mask =>
      val currentCombo = allPromotions.zipWithIndex.filter {
        case (_, index) => (mask & (1 << index)) != 0
      }.map(_._1.code)

      val isInvalidCombo = currentCombo.exists { code =>
        allPromotions.find(_.code == code).exists(_.notCombinableWith.intersect(currentCombo).nonEmpty)
      }

      if (!isInvalidCombo) Some(PromotionCombo(currentCombo)) else None
    }
    .filter(_.promotionCodes.length >= 2)
    .sortBy(- _.promotionCodes.length)

  sortedCombos.foldLeft(Seq.empty[PromotionCombo]) { (acc, combo) =>
    if (acc.exists(c => combo.promotionCodes.toSet.subsetOf(c.promotionCodes.toSet))) acc
    else acc :+ combo
  }
}

def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
  allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
}

@main def runProblemTwo(): Unit =
  val promo1: Promotion = Promotion("P1", Seq("P3"))
  val promo2: Promotion = Promotion("P2", Seq("P4", "P5"))
  val promo3: Promotion = Promotion("P3", Seq("P1"))
  val promo4: Promotion = Promotion("P4", Seq("P2"))
  val promo5: Promotion = Promotion("P5", Seq("P2"))

  val promoList: Seq[Promotion] = Seq(promo1, promo2, promo3, promo4, promo5)

  val validCombos = allCombinablePromotions(promoList)
  println(validCombos)

  val combinableP1 = combinablePromotions("P1", promoList)
  println(combinableP1)

  val combinableP3 = combinablePromotions("P3", promoList)
  println(combinableP3)