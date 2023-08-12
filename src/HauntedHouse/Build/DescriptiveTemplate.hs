module HauntedHouse.Build.DescriptiveTemplate where 

import HauntedHouse.Build.Template ( labelTemplate )
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Tokenizer.Data ( Lexeme (..) )
import HauntedHouse.Build.Descriptives
import HauntedHouse.Recognizer (Adjective)

foldMapM (uncurry (labelTemplate "Adjective")) descriptivesLabelValuePair