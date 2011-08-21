data Material
    = Material
    { mtCull    :: Maybe Bool -- none, front, back
    , mtSort    :: Int
    , mtStages  :: [StageMaterial]
    }

data LevelMaterial
    = LevelMaterial
--    { lm
--    }

data StageMaterial
    = StageMaterial
    { smAnimMap     :: [ByteString]
    , smBlend       :: ()
    , smDepthFunc   :: Maybe ()
    , smAlphaFunc   :: ()
    }

isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b 
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    VisData nvecs szvecs vecs = blVisData bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs UV.! i

findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = (blNodes bl) V.! i
    f:.b:.()= ndChildren node 
    plane   = (blPlanes bl) V.! (ndPlane node)
    dist    = ((plNormal plane) `dot` (camPos)) - plDist plane


data Frustum
    = Frustum
    { frPlanes :: Vec6 Plane
    , ntl :: Vec3f
    , ntr :: Vec3f
    , nbl :: Vec3f
    , nbr :: Vec3f
    , ftl :: Vec3f
    , ftr :: Vec3f
    , fbl :: Vec3f
    , fbr :: Vec3f
    }

pointInFrustum p fr = Vec.foldr (\(Plane n d) b -> b && d + n `dot` p >= 0) True $ frPlanes fr

sphereInFrustum p r fr = Vec.foldr (\(Plane n d) b -> b && d + n `dot` p >= (-r)) True $ frPlanes fr

boxInFrustum pp pn fr = Vec.foldr (\(Plane n d) b -> b && d + n `dot` (g pp pn n) >= 0) True $ frPlanes fr
  where
    g (px:.py:.pz:.()) (nx:.ny:.nz:.()) n = (fx px nx):.(fy py ny):.(fz pz nz):.()
      where
        fx:.fy:.fz:.() = Vec.map (\a -> if a > 0 then max else min) n

frustum :: Float -> Float -> Float -> Float -> Vec3f -> Vec3f -> Vec3f -> Frustum
frustum angle ratio nearD farD p l u = Frustum ((pl ntr ntl ftl):.(pl nbl nbr fbr):.(pl ntl nbl fbl):.
                                                (pl nbr ntr fbr):.(pl ntl ntr nbr):.(pl ftr ftl fbl):.()) ntl ntr nbl nbr ftl ftr fbl fbr
  where
    pl a b c = Plane n d
      where
        n = normalize $ (c - b) `cross` (a - b)
        d = -(n `dot` b)
    m a v = Vec.map (*a) v
    ang2rad = pi / 180
    tang    = tan $ angle * ang2rad * 0.5
    nh  = nearD * tang
    nw  = nh * ratio
    fh  = farD * tang
    fw  = fh * ratio
    z   = normalize $ p - l
    x   = normalize $ u `cross` z
    y   = z `cross` x

    nc  = p - m nearD z
    fc  = p - m farD z

    ntl = nc + m nh y - m nw x
    ntr = nc + m nh y + m nw x
    nbl = nc - m nh y - m nw x
    nbr = nc - m nh y + m nw x

    ftl = fc + m fh y - m fw x
    ftr = fc + m fh y + m fw x
    fbl = fc - m fh y - m fw x
    fbr = fc - m fh y + m fw x
