module Build.DescriptiveTemplate where 

import Build.Template ( labelTemplate )
import Game.Model.Mapping (Label (..))
import Tokenizer.Data ( Lexeme (..) )
import Build.Descriptives
import Recognizer (Adjective)

foldMapM (uncurry (labelTemplate "Adjective")) descriptivesLabelValuePair