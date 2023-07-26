module HauntedHouse.Game.Pregenerated.ObjectGIDs where 

cabinets :: (ObjectLabel,[GID Object])
cabinets = (cabinetLabel,[cabinetGID0, cabinetGID1, GID 2, GID 3])

cabinetGID0 :: GID Object
cabinetGID0 = GID 0

cabinetGID1 :: GID Object
cabinetGID1 = GID 1

cabinetGID2 :: GID Object 
cabinetGID2 = GID 2 

cabinetGID3 :: GID Object
cabinetGID3 = GID 3

shelf :: (ObjectLabel, [GID Object])
shelf = (shelfLabel, [GID 4])

sink :: (ObjectLabel, [GID Object])
sink = (sinkLabel, [GID 5])