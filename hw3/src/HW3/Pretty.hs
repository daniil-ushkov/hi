module HW3.Pretty (prettyValue) where
  
import HW3.Base (HiFun (..), HiValue (..))

import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter.Render.Terminal.Internal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
