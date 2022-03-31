
module Futhark.Config where
import qualified Futhark.Raw as Raw
import Foreign.C
import Foreign.CUDA.Ptr(DevicePtr(..))


data ContextOption
    = NvrtcOptions [String]
    | Debug Int
    | Log Int
    | Device String
    | LoadProgram String
    | DumpProgram String
    | LoadPtx String
    | DumpPtx String
    | DefaultGroupSize Int
    | DefaultGroupNum Int
    | DefaultTileSize Int
    | DefaultThreshold Int
    | Size String CSize

setOption config option = case option of
    (NvrtcOptions os)    -> mapM_ (\o -> withCString o $ Raw.context_config_add_nvrtc_option config) os
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
    (Device s)           -> withCString s $ Raw.context_config_set_device   config
    (LoadProgram s)      -> withCString s $ Raw.context_config_load_program_from config
    (DumpProgram s)      -> withCString s $ Raw.context_config_dump_program_to   config
    (LoadPtx s)          -> withCString s $ Raw.context_config_load_ptx_from     config
    (DumpPtx s)          -> withCString s $ Raw.context_config_dump_ptx_to       config
    (DefaultGroupSize s) -> Raw.context_config_set_default_group_size config s
    (DefaultGroupNum n)  -> Raw.context_config_set_default_num_groups config n
    (DefaultTileSize s)  -> Raw.context_config_set_default_tile_size  config s
    (DefaultThreshold n) -> Raw.context_config_set_default_threshold  config n
    (Size name s)        -> withCString name $ \n -> Raw.context_config_set_tuning_param config n s
                                           >>= \code -> if code == 0
                                                           then return ()
                                                           else error "invalid size"
