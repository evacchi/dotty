package scala.tasty

import scala.tasty.reflect._

abstract class Reflection
    extends Core
    with CaseDefOps
    with ConstantOps
    with ContextOps
    with IdOps
    with ImportSelectorOps
    with QuotedOps
    with PatternOps
    with PositionOps
    with Printers
    with SettingsOps
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TreeUtils
    with TypeOrBoundsTreeOps
    with TypeOrBoundsOps
