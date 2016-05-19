#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#
#    * Neither the name of ZanyBlue nor the names of its contributors may
#      be used to endorse or promote products derived from this software
#      without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Regeneration of the CLDR data
TOP=../..
ZBMCOMPILE=$(realpath $(TOP)/bin/zbmcompile$E)
#ASCII_ONLY=-A
ASCII_ONLY=
ZBMCFLAGS+=$(ASCII_ONLY)
ifeq ($(ASCII_ONLY),-A)
   # Disable inclusion of text of messages in generated files in ASCII mode
   ZBMCFLAGS+=-C
endif
# Optimize for locale based access
ZBMCFLAGS+=-O
# Verbose
ZBMCFLAGS+=-v
# Defined the message directory
ZBMCFLAGS+=-d $(MESGDIR)
MKCODECSMAPS=$(TOP)/src/bin/mkcodecsmaps.py
CLDR2LOCALES=$(TOP)/src/bin/cldr2locales.py
CLDR_PACKAGE=ZanyBlue.Text.CLDR_Data
LOCALES_SOURCE=zanyblue-text-locales.adb
MESGDIR=mesg
CLDR_DIR=$(TOP)/libs3rd/cldr
FACILTIES=l s t
BASE_LOCALE=en
LOCALES=ar cs da de el en en_AU en_CA en_IE en_GB en_NZ en_ZA es fi fr ga he \
	hu it ja ko nb nl pl pt ro ru sk sv th tr zh zh_Hant
# The following CLDR locales were excluding
#    en_Dsrt_US
#    en_Dsrt
#    en_Shaw
# As the required data was beyond Wide_Character (in the
# Wide_Wide_Character range).
ALL_LOCALES=aa_DJ aa_ER aa_ET aa af_NA af af_ZA ak_GH ak am_ET am ar_AE \
	ar_BH ar_DZ ar_EG ar_IQ ar_JO ar_KW ar_LB ar_LY ar_MA ar_OM \
	ar_QA ar_SA ar_SD ar_SY ar_TN ar ar_YE asa_TZ asa as_IN as \
	az_Arab_IR az_Arab az_AZ az_Cyrl_AZ az_Cyrl az_IR az_Latn_AZ \
	az_Latn az be_BY bem bem_ZM be bez_TZ bez bg_BG bg bm_ML bm \
	bn_BD bn_IN bn bo_CN bo_IN bo br_FR br bs_BA bs byn_ER byn \
	ca_ES ca cch_NG cch cgg_UG cgg chr_US chr cs_CZ cs cy_GB cy \
	da_DK dav_KE dav da de_AT de_BE de_CH de_DE de_LI de_LU de \
	dv_MV dv dz_BT dz ebu_KE ebu ee_GH ee_TG ee el_CY el_GR el \
	en_AS en_AU en_BE en_BW en_BZ en_CA en_GB \
	en_GU en_HK en_IE en_IN en_JM en_MH en_MP en_MT en_MU en_NA \
	en_NZ en_PH en_PK en_SG en_TT en_UM en_US en_VI en \
	en_ZA en_ZW eo es_419 es_AR es_BO es_CL es_CO es_CR es_DO \
	es_EC es_ES es_GQ es_GT es_HN es_MX es_NI es_PA es_PE es_PR \
	es_PY es_SV es_US es_UY es_VE es et_EE et eu_ES eu fa_AF \
	fa_IR fa ff_SN ff fi_FI fil_PH fil fi fo_FO fo fr_BE fr_BL \
	fr_CA fr_CF fr_CH fr_CI fr_CM fr_FR fr_GN fr_GP fr_LU fr_MC \
	fr_MF fr_MG fr_ML fr_MQ fr_NE fr_RE fr_SN fr fur_IT fur gaa_GH \
	gaa ga_IE ga gez_ER gez_ET gez gl_ES gl gsw_CH gsw gu_IN gu \
	guz_KE guz gv_GB gv ha_Arab_NG ha_Arab_SD ha_Arab ha_GH \
	ha_Latn_GH ha_Latn_NE ha_Latn_NG ha_Latn ha_NE ha_NG ha_SD \
	haw_US haw ha he_IL he hi_IN hi hr_HR hr hu_HU hu hy_AM hy \
	ia id_ID id ig_NG ig ii_CN ii in is_IS is it_CH it_IT it iu\
	ja_JP ja jmc_TZ jmc kab_DZ kab ka_GE kaj_NG kaj kam_KE kam \
	ka kcg_NG kcg kde_TZ kde kea_CV kea kfo_CI kfo khq_ML khq ki_KE \
	ki kk_Cyrl_KZ kk_Cyrl kk_KZ kk kl_GL kln_KE kln kl km_KH km \
	kn_IN kn kok_IN ko_KR kok ko kpe_GN kpe_LR kpe ksb_TZ ksb ksh_DE \
	ksh ku_Arab_IQ ku_Arab_IR ku_Arab_SY ku_Arab ku_IQ ku_IR \
	ku_Latn_SY ku_Latn_TR ku_Latn ku_SY ku_TR ku kw_GB kw ky_KG ky \
	lag_TZ lag lg_UG lg ln_CD ln_CG ln lo_LA lo lt_LT lt luo_KE luo \
	luy_KE luy lv_LV lv mas_KE mas_TZ mas mer_KE mer mfe_MU mfe mg_MG \
	mg mi_NZ mi mk_MK mk ml_IN ml mn_CN mn_Cyrl_MN mn_Cyrl mn_MN \
	mn_Mong_CN mn_Mong mn mo mr_IN mr ms_BN ms_MY ms mt_MT mt my_MM \
	my naq_NA naq nb_NO nb nds_DE nds nd nd_ZW ne_IN ne_NP ne nl_BE \
	nl_NL nl nn_NO nn nr nr_ZA nso nso_ZA ny_MW nyn_UG nyn ny oc_FR \
	oc om_ET om_KE om or_IN or pa_Arab_PK pa_Arab pa_Guru_IN pa_Guru \
	pa_IN pa_PK pa pl_PL pl ps_AF ps pt_BR pt_GW pt_MZ pt_PT pt rm_CH \
	rm rof_TZ rof ro_MD root ro_RO ro ru_MD ru_RU ru_UA ru rwk_TZ rwk \
	rw_RW rw sa_IN saq_KE saq sa se_FI seh_MZ seh se_NO ses_ML ses se \
	sg_CF sg sh_BA sh_CS shi_Latn_MA shi_Latn shi_MA shi_Tfng_MA \
	shi_Tfng shi sh sh_YU sid_ET sid si_LK si sk_SK sk sl_SI sl sn \
	sn_ZW so_DJ so_ET so_KE so_SO so sq_AL sq sr_BA sr_CS sr_Cyrl_BA \
	sr_Cyrl_CS sr_Cyrl_ME sr_Cyrl_RS sr_Cyrl sr_Cyrl_YU sr_Latn_BA \
	sr_Latn_CS sr_Latn_ME sr_Latn_RS sr_Latn sr_Latn_YU sr_ME sr_RS \
	sr sr_YU ss_SZ ss ssy_ER ssy ss_ZA st_LS st st_ZA sv_FI sv_SE sv \
	sw_KE sw_TZ sw syr_SY syr ta_IN ta_LK ta te_IN teo_KE teo_UG teo \
	te tg_Cyrl_TJ tg_Cyrl tg_TJ tg th_TH th ti_ER ti_ET tig_ER tig ti \
	tl tn tn_ZA to_TO to tr_TR trv_TW trv tr ts ts_ZA tt_RU tt \
	tzm_Latn_MA tzm_Latn tzm_MA tzm ug_Arab_CN ug_Arab ug_CN ug uk_UA \
	uk ur_IN ur_PK ur uz_AF uz_Arab_AF uz_Arab uz_Cyrl_UZ uz_Cyrl \
	uz_Latn_UZ uz_Latn uz_UZ uz ve ve_ZA vi_VN vi vun_TZ vun wal_ET wal \
	wo_Latn_SN wo_Latn wo_SN wo xh xh_ZA xog_UG xog yo_NG yo zh_CN \
	zh_Hans_CN zh_Hans_HK zh_Hans_MO zh_Hans_SG zh_Hans zh_Hant_HK \
	zh_Hant_MO zh_Hant_TW zh_Hant zh_HK zh_MO zh_SG zh_TW zh zu zu_ZA
ENCODING_ALIASES+=-A SHIFT_JIS=CP932
ENCODING_ALIASES+=-A GB2312=CP936
ENCODING_ALIASES+=-A BIG5=CP950
ENCODING_ALIASES+=-A CP28591=ASCII
ENCODING_ALIASES+=-A CP28592=ISO8859-2
ENCODING_ALIASES+=-A CP28593=ISO8859-3
ENCODING_ALIASES+=-A CP28594=ISO8859-4
ENCODING_ALIASES+=-A CP28595=ISO8859-5
ENCODING_ALIASES+=-A CP28596=ISO8859-6
ENCODING_ALIASES+=-A CP28597=ISO8859-7
ENCODING_ALIASES+=-A CP28598=ISO8859-8
ENCODING_ALIASES+=-A CP28599=ISO8859-9
ENCODING_ALIASES+=-A CP28603=ISO8859-13
ENCODING_ALIASES+=-A CP28605=ISO8859-15
MKCODECSMAPS_FLAGS+=-K
MKCODECSMAPS_FLAGS+=$(ENCODING_ALIASES)
MKCODECSMAPS_FLAGS+=$(MISC_ENCODINGS)
MKCODECSMAPS_FLAGS+=$(EBCDIC_ENCODINGS)

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk
include $(TOP)/src/mkfile/rules.mk

all:
	$(warning )
	$(warning This Makefile is only used to regenerate the compiled)
	$(warning locale data file and codecs information.  Use)
	$(warning )
	$(warning     make regenerate)
	$(warning )
	$(warning to perform this regeneration after a build, the zbmcompile)
	$(warning utility is needed.)
	$(warning )
	$(warning To regenerate just the CLDR data, use)
	$(warning )
	$(warning     make cldr)
	$(warning )
	$(warning To regenerate just the Codecs data, use)
	$(warning )
	$(warning     make codecs)
	$(warning )

regenerate:	cldr codecs

cldr:	properties zbmcompile
	$(ZBMCOMPILE) $(ZBMCFLAGS) $(CLDR_PACKAGE) $(FACILTIES)

properties:
	$(PYTHON) $(CLDR2LOCALES) $(ASCII_ONLY) -v -d $(CLDR_DIR) -B $(BASE_LOCALE) -p $(MESGDIR) -e $(LOCALES_SOURCE) $(LOCALES)

fullset:
	$(MAKE) LOCALES="$(ALL_LOCALES)" regenerate

zbmcompile: $(ZBMCOMPILE)

$(ZBMCOMPILE):
	$(warning The executable '$(ZBMCOMPILE)' is not present)
	$(warning Need to perform a build before regenerating locale data)
	$(error Please rebuild)

codecs:
	$(MKCODECSMAPS) $(MKCODECSMAPS_FLAGS)
