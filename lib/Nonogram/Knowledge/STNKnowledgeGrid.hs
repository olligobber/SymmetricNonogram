module Nonogram.Knowledge.STNKnowledgeGrid (
	STNKnowledgeGrid,
	) where

newtype STNKnowledgeGrid x = STNKnowledgeGrid
	( Dimensions -> forall s. STArray s Coordinate Knowledge ->
		ST s [(STArray s Coordinate Knowledge, x)]
	)

instance Functor STNKnowledgeGrid where
	fmap f (STNKnowledgeGrid x) =
		STNKnowledgeGrid $ fmap (fmap $ fmap $ fmap $ fmap f) x

instance Applicative STNKnowledgeGrid where
	pure x = STNKnowledgeGrid $ \_ r -> pure [(r, x)]
	STNKnowledgeGrid f <*> STNKnowledgeGrid x = STNKnowledgeGrid $ \d r -> do
		fs <- f d r
		concat <$> traverse (\(fr,ff) -> x d fr) fs

instance Monad STNKnowledgeGrid where
	STNKnowledgeGrid x >>= f = STNKnowledgeGrid $ \d r -> do
		xs <- x d r
		concat <$> traverse
			(\(xr,xx) -> let STNKnowledgeGrid fx = f xx in
				fx d xr
			)
			xs

instance MonadError STNKnowledgeGrid where
	error = STNKnowledgeGrid $ pure $ pure $ pure []

instance KnowledgeGrid STNKnowledgeGrid where
	readCell c = STNKnowledgeGrid $ \_ r -> do
		k <- readArray r c
		pure [(r, k)]
	writeCell c k = STNKnowledgeGrid $ \_ r -> do
		writeArray r c k
		pure [(r, ())]
	getUnknownOrSolution = STNKnowledgeGrid $ \d r -> do
		assocs <- getAssocs r
		let
			result = fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
				(\(c,k) -> case k of
					Empty -> pure mempty
					Filled -> pure $ S.singleton c
					Unknown -> Left c
				)
				assocs
				)
		pure [(r, result)]

instance NKnowledgeGrid STNKnowledgeGrid where
	tryAll c = STNKnowledgeGrid $ \d r -> do
		k <- readArray r c
		if k /= Unknown then
			pure [(r, ())]
		else do
			bounds <- getBounds r
			newr <- newGenArray bounds (readArray r)
			writeArray r c Empty
			writeArray newr c Filled
			pure [(r, ()), (newr, ())]