package com.myway.codinggame.solo.easy

object PeriodicTableSpelling {
  val elements = List(
    "H",
    "He",
    "Li",
    "Be",
    "B",
    "C",
    "N",
    "O",
    "F",
    "Ne",
    "Na",
    "Mg",
    "Al",
    "Si",
    "P",
    "S",
    "Cl",
    "Ar",
    "K",
    "Ca",
    "Sc",
    "Ti",
    "V",
    "Cr",
    "Mn",
    "Fe",
    "Co",
    "Ni",
    "Cu",
    "Zn",
    "Ga",
    "Ge",
    "As",
    "Se",
    "Br",
    "Kr",
    "Rb",
    "Sr",
    "Y",
    "Zr",
    "Nb",
    "Mo",
    "Tc",
    "Ru",
    "Rh",
    "Pd",
    "Ag",
    "Cd",
    "In",
    "Sn",
    "Sb",
    "Te",
    "I",
    "Xe",
    "Cs",
    "Ba",
    "La",
    "Ce",
    "Pr",
    "Nd",
    "Pm",
    "Sm",
    "Eu",
    "Gd",
    "Tb",
    "Dy",
    "Ho",
    "Er",
    "Tm",
    "Yb",
    "Lu",
    "Hf",
    "Ta",
    "W",
    "Re",
    "Os",
    "Ir",
    "Pt",
    "Au",
    "Hg",
    "Tl",
    "Pb",
    "Bi",
    "Po",
    "At",
    "Rn",
    "Fr",
    "Ra",
    "Ac",
    "Th",
    "Pa",
    "U",
    "Np",
    "Pu",
    "Am",
    "Cm",
    "Bk",
    "Cf",
    "Es",
    "Fm",
    "Md",
    "No",
    "Lr",
    "Rf",
    "Db",
    "Sg",
    "Bh",
    "Hs",
    "Mt",
    "Ds",
    "Rg",
    "Cn",
    "Nh",
    "Fl",
    "Mc",
    "Lv",
    "Ts",
    "Og"
  )

  def solve(s: String): List[String] = {
    val lowerToUpper: Map[String, String] = elements.map(x => (x.toLowerCase, x)).toMap

    go(
      lowerToUpper,
      (s.toLowerCase, Nil) :: Nil,
      Nil
    ).toSet.toList.sorted
  }

  def go(
    lowerToUpper: Map[String, String],
    l: List[(String, List[String])],
    out: List[String]
  ): List[String] = {

    val o: List[(String, List[String])] = for {
      (lower, prefixes)  <- l
      (remain, complete) <- findPrefix(lowerToUpper)(lower)
    } yield (remain, prefixes ++ List(complete))

    val complete: List[String] = out ++ o.filter(_._1.isEmpty).map(_._2.mkString)

    val incomplete: List[(String, List[String])] = o.filterNot(_._1.isEmpty)
    if (incomplete.isEmpty) complete
    else go(lowerToUpper, incomplete, complete)
  }

  private def findPrefix(lowerToUpper: Map[String, String]): String => List[(String, String)] = s =>
    for {
      key <- lowerToUpper.keySet.toList
      if s.startsWith(key)
    } yield (s.drop(key.length), lowerToUpper(key))

}
