MODULE agrif_profiles

  PUBLIC

#if defined key_agrif

INTEGER :: glamt_id, glamu_id, glamv_id,glamf_id
INTEGER :: gphit_id, gphiu_id, gphiv_id,gphif_id
INTEGER :: e1t_id, e1u_id, e1v_id, e1f_id
INTEGER :: e2t_id, e2u_id, e2v_id, e2f_id
INTEGER :: e1e2t_upd_id
INTEGER :: e1e2t_frac_id, e2u_frac_id, e1v_frac_id

INTEGER :: bathy_id, ht0_id

! Vertical scale factors

INTEGER :: e3t_id
INTEGER :: e3w_id
INTEGER :: e3t_copy_id
INTEGER :: e3t_connect_id
INTEGER :: e3u_id, e3v_id, e3f_id
INTEGER :: e3uw_id, e3vw_id

! Bottom level
INTEGER :: mbkt_id, mbku_id, mbkv_id, mbkf_id

# endif
END MODULE agrif_profiles
