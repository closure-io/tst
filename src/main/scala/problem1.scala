case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

// less efficient since it uses nested for loops, first attempt
def getBestGroupPriceNestedFor(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

  val uniqueRateGroups: Seq[String] = rates.map(_.rateGroup).distinct
  val uniqueCabinCodes: Seq[String] = prices.map(_.cabinCode).distinct

  for {
    cabinCode <- uniqueCabinCodes
    rateGroup <- uniqueRateGroups
  } yield {
    val relevantPrices = prices.filter(cp => cp.cabinCode == cabinCode && rates.exists(r => r.rateGroup == rateGroup && r.rateCode == cp.rateCode))

    val bestPriceOption = relevantPrices.minByOption(_.price)

    bestPriceOption match {
      case Some(bestPrice) => BestGroupPrice(cabinCode, bestPrice.rateCode, bestPrice.price, rateGroup)
      case None => BestGroupPrice(cabinCode, "N/A", -1.0, rateGroup)
    }
  }
}

def getBestGroupPrice(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
  val rateMap: Map[String, String] = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap

  prices.map(price => BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateMap(price.rateCode)))
    .groupBy(bestGroupPrice => (bestGroupPrice.cabinCode, bestGroupPrice.rateGroup))
    .map(bestGroupPrice => bestGroupPrice._2.minBy(grouping => grouping.price))
    .toSeq
}

@main def runProblemOne(): Unit =
  val rate1: Rate = Rate("M1", "Military")
  val rate2: Rate = Rate("M2", "Military")
  val rate3: Rate = Rate("S1", "Senior")
  val rate4: Rate = Rate("S2", "Senior")

  val rates: Seq[Rate] = Seq(rate1, rate2, rate3, rate4)

  val cab1: CabinPrice = CabinPrice("CA", "M1", 200.00)
  val cab2: CabinPrice = CabinPrice("CA", "M2", 250.00)
  val cab3: CabinPrice = CabinPrice("CA", "S1", 225.00)
  val cab4: CabinPrice = CabinPrice("CA", "S2", 260.00)
  val cab5: CabinPrice = CabinPrice("CB", "M1", 230.00)
  val cab6: CabinPrice = CabinPrice("CB", "M2", 260.00)
  val cab7: CabinPrice = CabinPrice("CB", "S1", 245.00)
  val cab8: CabinPrice = CabinPrice("CB", "S2", 270.00)

  val cabinPrices: Seq[CabinPrice] = Seq(cab1, cab2, cab3, cab4, cab5, cab6, cab7, cab8)

  val bestGroupPrices = getBestGroupPrice(rates, cabinPrices)
  for groupPrice <- bestGroupPrices do
    println(groupPrice)