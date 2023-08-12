module HauntedHouse.Build.DescriptiveTemplate where 

import HauntedHouse.Build.Template ( labelTemplate )
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Game.Model.World (Descriptive)
import HauntedHouse.Tokenizer.Data ( Lexeme (..) )
import HauntedHouse.Build.Descriptives

foldMapM (uncurry (labelTemplate "Descriptive")) descriptivesLabelValuePair