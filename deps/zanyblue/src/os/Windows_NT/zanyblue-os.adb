--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with GNAT.OS_Lib;
with Ada.Directories;
with Ada.Characters.Conversions;
with Interfaces.C;
with ZanyBlue.Text;
with ZanyBlue.Wide_Directories;

package body ZanyBlue.OS is

   use Ada.Directories;
   use ZanyBlue.Text;
   use ZanyBlue.Wide_Directories;

   use type Interfaces.C.unsigned_long;

   subtype LCID is Interfaces.C.unsigned_long;
   subtype CPID is Interfaces.C.unsigned_long;

   function GetUserDefaultLCID return LCID;
   pragma Import (Stdcall, GetUserDefaultLCID, "GetUserDefaultLCID");
   --  Return the Windows LCID value for the current user.

   function GetACP return CPID;
   pragma Import (Stdcall, GetACP, "GetACP");
   -- Return current code page id

   type String_Access is access constant Wide_String;
   type LCID_Map_Type is
      record
         Value : LCID;
         Name  : String_Access;
      end record;

   --  The following mapping from LCID values to locales is taken from the
   --  CLDR data "docs/design/C-sharp-LocaleNames.htm"

   --  العربية
   L_ar        : aliased constant Wide_String := "ar";
   --  العربية (المملكة العربية السعودية)
   L_ar_SA     : aliased constant Wide_String := "ar_SA";
   --  العربية (العراق)
   L_ar_IQ     : aliased constant Wide_String := "ar_IQ";
   --  العربية (مصر)
   L_ar_EG     : aliased constant Wide_String := "ar_EG";
   --  العربية (ليبيا)
   L_ar_LY     : aliased constant Wide_String := "ar_LY";
   --  العربية (الجزائر)
   L_ar_DZ     : aliased constant Wide_String := "ar_DZ";
   --  العربية (المملكة المغربية)
   L_ar_MA     : aliased constant Wide_String := "ar_MA";
   --  العربية (تونس)
   L_ar_TN     : aliased constant Wide_String := "ar_TN";
   --  العربية (عمان)
   L_ar_OM     : aliased constant Wide_String := "ar_OM";
   --  العربية (اليمن)
   L_ar_YE     : aliased constant Wide_String := "ar_YE";
   --  العربية (سوريا)
   L_ar_SY     : aliased constant Wide_String := "ar_SY";
   --  العربية (الأردن)
   L_ar_JO     : aliased constant Wide_String := "ar_JO";
   --  العربية (لبنان)
   L_ar_LB     : aliased constant Wide_String := "ar_LB";
   --  العربية (الكويت)
   L_ar_KW     : aliased constant Wide_String := "ar_KW";
   --  العربية (الإمارات العربية المتحدة)
   L_ar_AE     : aliased constant Wide_String := "ar_AE";
   --  العربية (البحرين)
   L_ar_BH     : aliased constant Wide_String := "ar_BH";
   --  العربية (قطر)
   L_ar_QA     : aliased constant Wide_String := "ar_QA";
   --  български
   L_bg        : aliased constant Wide_String := "bg";
   --  български (България)
   L_bg_BG     : aliased constant Wide_String := "bg_BG";
   --  català
   L_ca        : aliased constant Wide_String := "ca";
   --  català (català)
   L_ca_ES     : aliased constant Wide_String := "ca_ES";
   --  中文(简体)
   L_zh_Hans   : aliased constant Wide_String := "zh_Hans";
   --  中文(繁體) (台灣)
   L_zh_TW     : aliased constant Wide_String := "zh_TW";
   --  中文(简体) (中华人民共和国)
   L_zh_CN     : aliased constant Wide_String := "zh_CN";
   --  中文(繁體) (香港特别行政区)
   L_zh_HK     : aliased constant Wide_String := "zh_HK";
   --  中文(简体) (新加坡)
   L_zh_SG     : aliased constant Wide_String := "zh_SG";
   --  中文(简体) (澳门特别行政区)
   L_zh_MO     : aliased constant Wide_String := "zh_MO";
   --  中文(繁體)
   L_zh_Hant   : aliased constant Wide_String := "zh_Hant";
   --  čeština
   L_cs        : aliased constant Wide_String := "cs";
   --  čeština (Česká republika)
   L_cs_CZ     : aliased constant Wide_String := "cs_CZ";
   --  dansk
   L_da        : aliased constant Wide_String := "da";
   --  dansk (Danmark)
   L_da_DK     : aliased constant Wide_String := "da_DK";
   --  Deutsch
   L_de        : aliased constant Wide_String := "de";
   --  Deutsch (Deutschland)
   L_de_DE     : aliased constant Wide_String := "de_DE";
   --  Deutsch (Schweiz)
   L_de_CH     : aliased constant Wide_String := "de_CH";
   --  Deutsch (Österreich)
   L_de_AT     : aliased constant Wide_String := "de_AT";
   --  Deutsch (Luxemburg)
   L_de_LU     : aliased constant Wide_String := "de_LU";
   --  Deutsch (Liechtenstein)
   L_de_LI     : aliased constant Wide_String := "de_LI";
   --  ελληνικά
   L_el        : aliased constant Wide_String := "el";
   --  ελληνικά (Ελλάδα)
   L_el_GR     : aliased constant Wide_String := "el_GR";
   --  English
   L_en        : aliased constant Wide_String := "en";
   --  English (United States)
   L_en_US     : aliased constant Wide_String := "en_US";
   --  English (United Kingdom)
   L_en_GB     : aliased constant Wide_String := "en_GB";
   --  English (Australia)
   L_en_AU     : aliased constant Wide_String := "en_AU";
   --  English (Canada)
   L_en_CA     : aliased constant Wide_String := "en_CA";
   --  English (New Zealand)
   L_en_NZ     : aliased constant Wide_String := "en_NZ";
   --  English (Eire)
   L_en_IE     : aliased constant Wide_String := "en_IE";
   --  English (South Africa)
   L_en_ZA     : aliased constant Wide_String := "en_ZA";
   --  English (Jamaica)
   L_en_JM     : aliased constant Wide_String := "en_JM";
   --  English (Caribbean)
   L_en_CB     : aliased constant Wide_String := "en_CB";
   --  English (Belize)
   L_en_BZ     : aliased constant Wide_String := "en_BZ";
   --  English (Trinidad y Tobago)
   L_en_TT     : aliased constant Wide_String := "en_TT";
   --  English (Zimbabwe)
   L_en_ZW     : aliased constant Wide_String := "en_ZW";
   --  English (Philippines)
   L_en_PH     : aliased constant Wide_String := "en_PH";
   --  español
   L_es        : aliased constant Wide_String := "es";
   --  Español (México)
   L_es_MX     : aliased constant Wide_String := "es_MX";
   --  español (España)
   L_es_ES     : aliased constant Wide_String := "es_ES";
   --  Español (Guatemala)
   L_es_GT     : aliased constant Wide_String := "es_GT";
   --  Español (Costa Rica)
   L_es_CR     : aliased constant Wide_String := "es_CR";
   --  Español (Panamá)
   L_es_PA     : aliased constant Wide_String := "es_PA";
   --  Español (República Dominicana)
   L_es_DO     : aliased constant Wide_String := "es_DO";
   --  Español (Republica Bolivariana de Venezuela)
   L_es_VE     : aliased constant Wide_String := "es_VE";
   --  Español (Colombia)
   L_es_CO     : aliased constant Wide_String := "es_CO";
   --  Español (Perú)
   L_es_PE     : aliased constant Wide_String := "es_PE";
   --  Español (Argentina)
   L_es_AR     : aliased constant Wide_String := "es_AR";
   --  Español (Ecuador)
   L_es_EC     : aliased constant Wide_String := "es_EC";
   --  Español (Chile)
   L_es_CL     : aliased constant Wide_String := "es_CL";
   --  Español (Uruguay)
   L_es_UY     : aliased constant Wide_String := "es_UY";
   --  Español (Paraguay)
   L_es_PY     : aliased constant Wide_String := "es_PY";
   --  Español (Bolivia)
   L_es_BO     : aliased constant Wide_String := "es_BO";
   --  Español (El Salvador)
   L_es_SV     : aliased constant Wide_String := "es_SV";
   --  Español (Honduras)
   L_es_HN     : aliased constant Wide_String := "es_HN";
   --  Español (Nicaragua)
   L_es_NI     : aliased constant Wide_String := "es_NI";
   --  Español (Puerto Rico)
   L_es_PR     : aliased constant Wide_String := "es_PR";
   --  suomi
   L_fi        : aliased constant Wide_String := "fi";
   --  suomi (Suomi)
   L_fi_FI     : aliased constant Wide_String := "fi_FI";
   --  français
   L_fr        : aliased constant Wide_String := "fr";
   --  français (France)
   L_fr_FR     : aliased constant Wide_String := "fr_FR";
   --  français (Belgique)
   L_fr_BE     : aliased constant Wide_String := "fr_BE";
   --  français (Canada)
   L_fr_CA     : aliased constant Wide_String := "fr_CA";
   --  français (Suisse)
   L_fr_CH     : aliased constant Wide_String := "fr_CH";
   --  français (Luxembourg)
   L_fr_LU     : aliased constant Wide_String := "fr_LU";
   --  français (Principauté de Monaco)
   L_fr_MC     : aliased constant Wide_String := "fr_MC";
   --  עברית
   L_he        : aliased constant Wide_String := "he";
   --  עברית (ישראל)
   L_he_IL     : aliased constant Wide_String := "he_IL";
   --  Magyar
   L_hu        : aliased constant Wide_String := "hu";
   --  Magyar (Magyarország)
   L_hu_HU     : aliased constant Wide_String := "hu_HU";
   --  íslenska
   L_is        : aliased constant Wide_String := "is";
   --  íslenska (Ísland)
   L_is_IS     : aliased constant Wide_String := "is_IS";
   --  italiano
   L_it        : aliased constant Wide_String := "it";
   --  italiano (Italia)
   L_it_IT     : aliased constant Wide_String := "it_IT";
   --  italiano (Svizzera)
   L_it_CH     : aliased constant Wide_String := "it_CH";
   --  日本語
   L_ja        : aliased constant Wide_String := "ja";
   --  日本語 (日本)
   L_ja_JP     : aliased constant Wide_String := "ja_JP";
   --  한국어
   L_ko        : aliased constant Wide_String := "ko";
   --  한국어 (대한민국)
   L_ko_KR     : aliased constant Wide_String := "ko_KR";
   --  Nederlands
   L_nl        : aliased constant Wide_String := "nl";
   --  Nederlands (Nederland)
   L_nl_NL     : aliased constant Wide_String := "nl_NL";
   --  Nederlands (België)
   L_nl_BE     : aliased constant Wide_String := "nl_BE";
   --  norsk
   L_no        : aliased constant Wide_String := "no";
   --  norsk (bokmål) (Norge)
   L_nb_NO     : aliased constant Wide_String := "nb_NO";
   --  norsk (nynorsk) (Noreg)
   L_nn_NO     : aliased constant Wide_String := "nn_NO";
   --  polski
   L_pl        : aliased constant Wide_String := "pl";
   --  polski (Polska)
   L_pl_PL     : aliased constant Wide_String := "pl_PL";
   --  Português
   L_pt        : aliased constant Wide_String := "pt";
   --  Português (Brasil)
   L_pt_BR     : aliased constant Wide_String := "pt_BR";
   --  português (Portugal)
   L_pt_PT     : aliased constant Wide_String := "pt_PT";
   --  română
   L_ro        : aliased constant Wide_String := "ro";
   --  română (România)
   L_ro_RO     : aliased constant Wide_String := "ro_RO";
   --  русский
   L_ru        : aliased constant Wide_String := "ru";
   --  русский (Россия)
   L_ru_RU     : aliased constant Wide_String := "ru_RU";
   --  hrvatski
   L_hr        : aliased constant Wide_String := "hr";
   --  hrvatski (Hrvatska)
   L_hr_HR     : aliased constant Wide_String := "hr_HR";
   --  srpski (Srbija)
   L_sr_Latn_SP : aliased constant Wide_String := "sr_Latn_SP";
   --  српски (Југославија)
   L_sr_Cyrl_SP : aliased constant Wide_String := "sr_Cyrl_SP";
   --  slovenčina
   L_sk        : aliased constant Wide_String := "sk";
   --  slovenčina (Slovenská republika)
   L_sk_SK     : aliased constant Wide_String := "sk_SK";
   --  shqipe
   L_sq        : aliased constant Wide_String := "sq";
   --  shqipe (Shqipëria)
   L_sq_AL     : aliased constant Wide_String := "sq_AL";
   --  svenska
   L_sv        : aliased constant Wide_String := "sv";
   --  svenska (Sverige)
   L_sv_SE     : aliased constant Wide_String := "sv_SE";
   --  svenska (Finland)
   L_sv_FI     : aliased constant Wide_String := "sv_FI";
   --  ไทย
   L_th        : aliased constant Wide_String := "th";
   --  ไทย (ไทย)
   L_th_TH     : aliased constant Wide_String := "th_TH";
   --  Türkçe
   L_tr        : aliased constant Wide_String := "tr";
   --  Türkçe (Türkiye)
   L_tr_TR     : aliased constant Wide_String := "tr_TR";
   --  ٱردو
   L_ur        : aliased constant Wide_String := "ur";
   --  اُردو (پاکستان)
   L_ur_PK     : aliased constant Wide_String := "ur_PK";
   --  Bahasa Indonesia
   L_id        : aliased constant Wide_String := "id";
   --  Bahasa Indonesia (Indonesia)
   L_id_ID     : aliased constant Wide_String := "id_ID";
   --  україньска
   L_uk        : aliased constant Wide_String := "uk";
   --  україньска (Україна)
   L_uk_UA     : aliased constant Wide_String := "uk_UA";
   --  Беларускі
   L_be        : aliased constant Wide_String := "be";
   --  Беларускі (Беларусь)
   L_be_BY     : aliased constant Wide_String := "be_BY";
   --  slovenski
   L_sl        : aliased constant Wide_String := "sl";
   --  slovenski (Slovenija)
   L_sl_SI     : aliased constant Wide_String := "sl_SI";
   --  eesti
   L_et        : aliased constant Wide_String := "et";
   --  eesti (Eesti)
   L_et_EE     : aliased constant Wide_String := "et_EE";
   --  latviešu
   L_lv        : aliased constant Wide_String := "lv";
   --  latviešu (Latvija)
   L_lv_LV     : aliased constant Wide_String := "lv_LV";
   --  lietuvių
   L_lt        : aliased constant Wide_String := "lt";
   --  lietuvių (Lietuva)
   L_lt_LT     : aliased constant Wide_String := "lt_LT";
   --  فارسى
   L_fa        : aliased constant Wide_String := "fa";
   --  فارسى (ايران)
   L_fa_IR     : aliased constant Wide_String := "fa_IR";
   --  Tiếng Việt Nam
   L_vi        : aliased constant Wide_String := "vi";
   --  Tiếng Việt Nam (Việt Nam)
   L_vi_VN     : aliased constant Wide_String := "vi_VN";
   --  Հայերեն
   L_hy        : aliased constant Wide_String := "hy";
   --  Հայերեն (Հայաստան)
   L_hy_AM     : aliased constant Wide_String := "hy_AM";
   --  Azərbaycan­ılı
   L_az        : aliased constant Wide_String := "az";
   --  Azərbaycan­ılı (Azərbaycanca)
   L_az_Latn_AZ : aliased constant Wide_String := "az_Latn_AZ";
   --  Азәрбајҹан (Азәрбајҹан)
   L_az_Cyrl_AZ : aliased constant Wide_String := "az_Cyrl_AZ";
   --  euskara
   L_eu        : aliased constant Wide_String := "eu";
   --  euskara (euskara)
   L_eu_ES     : aliased constant Wide_String := "eu_ES";
   --  македонски јазик
   L_mk        : aliased constant Wide_String := "mk";
   --  македонски јазик (Македонија)
   L_mk_MK     : aliased constant Wide_String := "mk_MK";
   --  Afrikaans
   L_af        : aliased constant Wide_String := "af";
   --  Afrikaans (Suid Afrika)
   L_af_ZA     : aliased constant Wide_String := "af_ZA";
   --  ქართული
   L_ka        : aliased constant Wide_String := "ka";
   --  ქართული (საქართველო)
   L_ka_GE     : aliased constant Wide_String := "ka_GE";
   --  føroyskt
   L_fo        : aliased constant Wide_String := "fo";
   --  føroyskt (Føroyar)
   L_fo_FO     : aliased constant Wide_String := "fo_FO";
   --  हिंदी
   L_hi        : aliased constant Wide_String := "hi";
   --  हिंदी (भारत)
   L_hi_IN     : aliased constant Wide_String := "hi_IN";
   --  Bahasa Malaysia
   L_ms        : aliased constant Wide_String := "ms";
   --  Bahasa Malaysia (Malaysia)
   L_ms_MY     : aliased constant Wide_String := "ms_MY";
   --  Bahasa Malaysia (Brunei Darussalam)
   L_ms_BN     : aliased constant Wide_String := "ms_BN";
   --  Қазащb
   L_kk        : aliased constant Wide_String := "kk";
   --  Қазақ (Қазақстан)
   L_kk_KZ     : aliased constant Wide_String := "kk_KZ";
   --  Кыргыз
   L_ky        : aliased constant Wide_String := "ky";
   --  Кыргыз (Кыргызстан)
   L_ky_KZ     : aliased constant Wide_String := "ky_KZ";
   --  Kiswahili
   L_sw        : aliased constant Wide_String := "sw";
   --  Kiswahili (Kenya)
   L_sw_KE     : aliased constant Wide_String := "sw_KE";
   --  U'zbek
   L_uz        : aliased constant Wide_String := "uz";
   --  U'zbek (U'zbekiston Respublikasi)
   L_uz_Latn_UZ : aliased constant Wide_String := "uz_Latn_UZ";
   --  Ўзбек (Ўзбекистон)
   L_uz_Cyrl_UZ : aliased constant Wide_String := "uz_Cyrl_UZ";
   --  Татар
   L_tt        : aliased constant Wide_String := "tt";
   --  Татар
   L_tt_RU     : aliased constant Wide_String := "tt_RU";
   --  ਪੰਜਾਬੀ
   L_pa        : aliased constant Wide_String := "pa";
   --  ਪੰਜਾਬੀ (ਭਾਰਤ)
   L_pa_IN     : aliased constant Wide_String := "pa_IN";
   --  ગુજરાતી
   L_gu        : aliased constant Wide_String := "gu";
   --  ગુજરાતી (ભારત)
   L_gu_IN     : aliased constant Wide_String := "gu_IN";
   --  தமிழ்
   L_ta        : aliased constant Wide_String := "ta";
   --  தமிழ் (இந்தியா)
   L_ta_IN     : aliased constant Wide_String := "ta_IN";
   --  తెలుగు
   L_te        : aliased constant Wide_String := "te";
   --  తెలుగు (భారత దేశం)
   L_te_IN     : aliased constant Wide_String := "te_IN";
   --  ಕನ್ನಡ
   L_kn        : aliased constant Wide_String := "kn";
   --  ಕನ್ನಡ (ಭಾರತ)
   L_kn_IN     : aliased constant Wide_String := "kn_IN";
   --  मराठी
   L_mr        : aliased constant Wide_String := "mr";
   --  मराठी (भारत)
   L_mr_IN     : aliased constant Wide_String := "mr_IN";
   --  संस्कृत
   L_sa        : aliased constant Wide_String := "sa";
   --  संस्कृत (भारतम्)
   L_sa_IN     : aliased constant Wide_String := "sa_IN";
   --  Монгол хэл
   L_mn        : aliased constant Wide_String := "mn";
   --  Монгол хэл (Монгол улс)
   L_mn_MN     : aliased constant Wide_String := "mn_MN";
   --  galego
   L_gl        : aliased constant Wide_String := "gl";
   --  galego (galego)
   L_gl_ES     : aliased constant Wide_String := "gl_ES";
   --  कोंकणी
   L_kok       : aliased constant Wide_String := "kok";
   --  कोंकणी (भारत)
   L_kok_IN    : aliased constant Wide_String := "kok_IN";
   --  ܣܘܪܝܝܐ
   L_syr       : aliased constant Wide_String := "syr";
   --  ܣܘܪܝܝܐ (سوريا)
   L_syr_SY    : aliased constant Wide_String := "syr_SY";
   --  ދިވެހިބަސް
   L_div       : aliased constant Wide_String := "div";
   --  ދިވެހިބަސް (ދިވެހި ރާއްޖެ)
   L_div_MV    : aliased constant Wide_String := "div_MV";
   --  Table mapping LCID value to name.  This table is sorted by LCID values.
   LCID_Mapping : constant array (Positive range <>) of LCID_Map_Type := (
                     (16#0001#, L_ar'Access),
                     (16#0002#, L_bg'Access),
                     (16#0003#, L_ca'Access),
                     (16#0004#, L_zh_Hans'Access),
                     (16#0005#, L_cs'Access),
                     (16#0006#, L_da'Access),
                     (16#0007#, L_de'Access),
                     (16#0008#, L_el'Access),
                     (16#0009#, L_en'Access),
                     (16#000A#, L_es'Access),
                     (16#000B#, L_fi'Access),
                     (16#000C#, L_fr'Access),
                     (16#000D#, L_he'Access),
                     (16#000E#, L_hu'Access),
                     (16#000F#, L_is'Access),
                     (16#0010#, L_it'Access),
                     (16#0011#, L_ja'Access),
                     (16#0012#, L_ko'Access),
                     (16#0013#, L_nl'Access),
                     (16#0014#, L_no'Access),
                     (16#0015#, L_pl'Access),
                     (16#0016#, L_pt'Access),
                     (16#0018#, L_ro'Access),
                     (16#0019#, L_ru'Access),
                     (16#001A#, L_hr'Access),
                     (16#001B#, L_sk'Access),
                     (16#001C#, L_sq'Access),
                     (16#001D#, L_sv'Access),
                     (16#001E#, L_th'Access),
                     (16#001F#, L_tr'Access),
                     (16#0020#, L_ur'Access),
                     (16#0021#, L_id'Access),
                     (16#0022#, L_uk'Access),
                     (16#0023#, L_be'Access),
                     (16#0024#, L_sl'Access),
                     (16#0025#, L_et'Access),
                     (16#0026#, L_lv'Access),
                     (16#0027#, L_lt'Access),
                     (16#0029#, L_fa'Access),
                     (16#002A#, L_vi'Access),
                     (16#002B#, L_hy'Access),
                     (16#002C#, L_az'Access),
                     (16#002D#, L_eu'Access),
                     (16#002F#, L_mk'Access),
                     (16#0036#, L_af'Access),
                     (16#0037#, L_ka'Access),
                     (16#0038#, L_fo'Access),
                     (16#0039#, L_hi'Access),
                     (16#003E#, L_ms'Access),
                     (16#003F#, L_kk'Access),
                     (16#0040#, L_ky'Access),
                     (16#0041#, L_sw'Access),
                     (16#0043#, L_uz'Access),
                     (16#0044#, L_tt'Access),
                     (16#0046#, L_pa'Access),
                     (16#0047#, L_gu'Access),
                     (16#0049#, L_ta'Access),
                     (16#004A#, L_te'Access),
                     (16#004B#, L_kn'Access),
                     (16#004E#, L_mr'Access),
                     (16#004F#, L_sa'Access),
                     (16#0050#, L_mn'Access),
                     (16#0056#, L_gl'Access),
                     (16#0057#, L_kok'Access),
                     (16#005A#, L_syr'Access),
                     (16#0065#, L_div'Access),
                     (16#0401#, L_ar_SA'Access),
                     (16#0402#, L_bg_BG'Access),
                     (16#0403#, L_ca_ES'Access),
                     (16#0404#, L_zh_TW'Access),
                     (16#0405#, L_cs_CZ'Access),
                     (16#0406#, L_da_DK'Access),
                     (16#0407#, L_de_DE'Access),
                     (16#0408#, L_el_GR'Access),
                     (16#0409#, L_en_US'Access),
                     (16#040B#, L_fi_FI'Access),
                     (16#040C#, L_fr_FR'Access),
                     (16#040D#, L_he_IL'Access),
                     (16#040E#, L_hu_HU'Access),
                     (16#040F#, L_is_IS'Access),
                     (16#0410#, L_it_IT'Access),
                     (16#0411#, L_ja_JP'Access),
                     (16#0412#, L_ko_KR'Access),
                     (16#0413#, L_nl_NL'Access),
                     (16#0414#, L_nb_NO'Access),
                     (16#0415#, L_pl_PL'Access),
                     (16#0416#, L_pt_BR'Access),
                     (16#0418#, L_ro_RO'Access),
                     (16#0419#, L_ru_RU'Access),
                     (16#041A#, L_hr_HR'Access),
                     (16#041B#, L_sk_SK'Access),
                     (16#041C#, L_sq_AL'Access),
                     (16#041D#, L_sv_SE'Access),
                     (16#041E#, L_th_TH'Access),
                     (16#041F#, L_tr_TR'Access),
                     (16#0420#, L_ur_PK'Access),
                     (16#0421#, L_id_ID'Access),
                     (16#0422#, L_uk_UA'Access),
                     (16#0423#, L_be_BY'Access),
                     (16#0424#, L_sl_SI'Access),
                     (16#0425#, L_et_EE'Access),
                     (16#0426#, L_lv_LV'Access),
                     (16#0427#, L_lt_LT'Access),
                     (16#0429#, L_fa_IR'Access),
                     (16#042A#, L_vi_VN'Access),
                     (16#042B#, L_hy_AM'Access),
                     (16#042C#, L_az_Latn_AZ'Access),
                     (16#042D#, L_eu_ES'Access),
                     (16#042F#, L_mk_MK'Access),
                     (16#0436#, L_af_ZA'Access),
                     (16#0437#, L_ka_GE'Access),
                     (16#0438#, L_fo_FO'Access),
                     (16#0439#, L_hi_IN'Access),
                     (16#043E#, L_ms_MY'Access),
                     (16#043F#, L_kk_KZ'Access),
                     (16#0440#, L_ky_KZ'Access),
                     (16#0441#, L_sw_KE'Access),
                     (16#0443#, L_uz_Latn_UZ'Access),
                     (16#0444#, L_tt_RU'Access),
                     (16#0446#, L_pa_IN'Access),
                     (16#0447#, L_gu_IN'Access),
                     (16#0449#, L_ta_IN'Access),
                     (16#044A#, L_te_IN'Access),
                     (16#044B#, L_kn_IN'Access),
                     (16#044E#, L_mr_IN'Access),
                     (16#044F#, L_sa_IN'Access),
                     (16#0450#, L_mn_MN'Access),
                     (16#0456#, L_gl_ES'Access),
                     (16#0457#, L_kok_IN'Access),
                     (16#045A#, L_syr_SY'Access),
                     (16#0465#, L_div_MV'Access),
                     (16#0801#, L_ar_IQ'Access),
                     (16#0804#, L_zh_CN'Access),
                     (16#0807#, L_de_CH'Access),
                     (16#0809#, L_en_GB'Access),
                     (16#080A#, L_es_MX'Access),
                     (16#080C#, L_fr_BE'Access),
                     (16#0810#, L_it_CH'Access),
                     (16#0813#, L_nl_BE'Access),
                     (16#0814#, L_nn_NO'Access),
                     (16#0816#, L_pt_PT'Access),
                     (16#081A#, L_sr_Latn_SP'Access),
                     (16#081D#, L_sv_FI'Access),
                     (16#082C#, L_az_Cyrl_AZ'Access),
                     (16#083E#, L_ms_BN'Access),
                     (16#0843#, L_uz_Cyrl_UZ'Access),
                     (16#0C01#, L_ar_EG'Access),
                     (16#0C04#, L_zh_HK'Access),
                     (16#0C07#, L_de_AT'Access),
                     (16#0C09#, L_en_AU'Access),
                     (16#0C0A#, L_es_ES'Access),
                     (16#0C0C#, L_fr_CA'Access),
                     (16#0C1A#, L_sr_Cyrl_SP'Access),
                     (16#1001#, L_ar_LY'Access),
                     (16#1004#, L_zh_SG'Access),
                     (16#1007#, L_de_LU'Access),
                     (16#1009#, L_en_CA'Access),
                     (16#100A#, L_es_GT'Access),
                     (16#100C#, L_fr_CH'Access),
                     (16#1401#, L_ar_DZ'Access),
                     (16#1404#, L_zh_MO'Access),
                     (16#1407#, L_de_LI'Access),
                     (16#1409#, L_en_NZ'Access),
                     (16#140A#, L_es_CR'Access),
                     (16#140C#, L_fr_LU'Access),
                     (16#1801#, L_ar_MA'Access),
                     (16#1809#, L_en_IE'Access),
                     (16#180A#, L_es_PA'Access),
                     (16#180C#, L_fr_MC'Access),
                     (16#1C01#, L_ar_TN'Access),
                     (16#1C09#, L_en_ZA'Access),
                     (16#1C0A#, L_es_DO'Access),
                     (16#2001#, L_ar_OM'Access),
                     (16#2009#, L_en_JM'Access),
                     (16#200A#, L_es_VE'Access),
                     (16#2401#, L_ar_YE'Access),
                     (16#2409#, L_en_CB'Access),
                     (16#240A#, L_es_CO'Access),
                     (16#2801#, L_ar_SY'Access),
                     (16#2809#, L_en_BZ'Access),
                     (16#280A#, L_es_PE'Access),
                     (16#2C01#, L_ar_JO'Access),
                     (16#2C09#, L_en_TT'Access),
                     (16#2C0A#, L_es_AR'Access),
                     (16#3001#, L_ar_LB'Access),
                     (16#3009#, L_en_ZW'Access),
                     (16#300A#, L_es_EC'Access),
                     (16#3401#, L_ar_KW'Access),
                     (16#3409#, L_en_PH'Access),
                     (16#340A#, L_es_CL'Access),
                     (16#3801#, L_ar_AE'Access),
                     (16#380A#, L_es_UY'Access),
                     (16#3C01#, L_ar_BH'Access),
                     (16#3C0A#, L_es_PY'Access),
                     (16#4001#, L_ar_QA'Access),
                     (16#400A#, L_es_BO'Access),
                     (16#440A#, L_es_SV'Access),
                     (16#480A#, L_es_HN'Access),
                     (16#4C0A#, L_es_NI'Access),
                     (16#500A#, L_es_PR'Access),
                     (16#7C04#, L_zh_Hant'Access));

   function LCID_To_Locale (Value : LCID) return Wide_String;
   --  Use the LCID_Mapping table to locate the name corresponding to an
   --  LCID value (simple binary search).  If not found, return the empty
   --  string.

   function Code_Page (CP : CPID) return Wide_String;
   -- Convert a code page id to a string code page name, e.g. 1252 => "CP1252"

   ---------------
   -- Code_Page --
   ---------------

   function Code_Page (CP : CPID) return Wide_String is
      CP_W : constant Wide_String := CPID'Wide_Image (CP);
   begin
      return "CP" & CP_W (CP_W'First + 1 .. CP_W'Last);
   end Code_Page;

   ---------------------
   -- Integrity_Check --
   ---------------------

   procedure Integrity_Check is
      LCID_Table_Not_Sorted_Error : exception;
      Current, Next : LCID;
   begin
      for I in LCID_Mapping'First ..  LCID_Mapping'Last - 1 loop
         Current := LCID_Mapping (I).Value;
         Next := LCID_Mapping (I + 1).Value;
         if Current > Next then
            raise LCID_Table_Not_Sorted_Error
               with "Index" & Positive'Image (I)
                           & ", mis-placed value" & LCID'Image (Next);
         end if;
      end loop;
   end Integrity_Check;

   --------------------
   -- LCID_To_Locale --
   --------------------

   function LCID_To_Locale (Value : LCID) return Wide_String is

      Left      : Positive := LCID_Mapping'First;
      Right     : Positive := LCID_Mapping'Last + 1;
      Center    : Positive;
      Candidate : LCID;

   begin
      if Value < LCID_Mapping (Left).Value then
         return "";
      end if;
      loop
         Center := Left + (Right - Left) / 2;
         Candidate := LCID_Mapping (Center).Value;
         if Value = Candidate then
            return LCID_Mapping (Center).Name.all;
         end if;

         if Right - Left <= 1 then
            return "";
         elsif Value < Candidate then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end LCID_To_Locale;

   --------------------
   -- OS_Locale_Name --
   --------------------

   function OS_Locale_Name return Wide_String is
   begin
      return LCID_To_Locale (GetUserDefaultLCID) & "." & Code_Page (GetACP);
   end OS_Locale_Name;

   --------------------
   -- OS_Locale_Name --
   --------------------

   function OS_Locale_Name return Wide_Wide_String is
      use Ada.Characters.Conversions;
   begin
      return To_Wide_Wide_String (OS_Locale_Name);
   end OS_Locale_Name;

   -------------
   -- OS_Name --
   -------------

   function OS_Name return OS_Name_Type is
   begin
      return Windows;
   end OS_Name;

   -----------------
   -- OS_New_Line --
   -----------------

   function OS_New_Line return Wide_String is
   begin
      return ""
         & Ada.Characters.Conversions.To_Wide_Character (ASCII.CR)
         & Ada.Characters.Conversions.To_Wide_Character (ASCII.LF);
   end OS_New_Line;

   -----------------
   -- OS_New_Line --
   -----------------

   function OS_New_Line return Wide_Wide_String is
      use Ada.Characters.Conversions;
   begin
      return To_Wide_Wide_String (OS_New_Line);
   end OS_New_Line;

   --------------------
   -- UTF8_File_Form --
   --------------------

   function UTF8_File_Form return String is
   begin
      return "WCEM=8";
   end UTF8_File_Form;

   --------------------
   -- Wide_Copy_Tree --
   --------------------

   procedure Wide_Copy_Tree (Source_Name : Wide_String;
                             Target_Name : Wide_String) is

      procedure Process_Entry (Path : String;
                               Elem : String;
                               Kind : File_Kind);

      procedure Process_Entry (Path : String;
                               Elem : String;
                               Kind : File_Kind) is

         Wide_Elem : constant Wide_String := Wide_From_UTF8 (Elem);
         Dest_Path : constant Wide_String := Wide_Compose (Target_Name,
                                                           Wide_Elem);
      begin
         if Elem'Length = 0 or else Elem (Elem'First) = '.' then
            return;
         end if;
         case Kind is
         when Ordinary_File =>
            Wide_Copy_File (Wide_From_UTF8 (Path), Dest_Path);
         when Directory =>
            Wide_Copy_Tree (Wide_From_UTF8 (Path), Dest_Path);
         when others =>
            null;
         end case;
      end Process_Entry;

      Item   : Directory_Entry_Type;
      Search : Search_Type;

   begin
      Wide_Create_Directory (Target_Name);
      Start_Search (Search, Wide_To_UTF8 (Source_Name), "*");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         Process_Entry (Full_Name (Item), Simple_Name (Item), Kind (Item));
      end loop;
      End_Search (Search);
   end Wide_Copy_Tree;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Text_IO.File_Type;
                          Name : Wide_String) is
      use Ada.Text_IO;
   begin
      Create (File,
              Mode => Out_File,
              Name => Wide_To_UTF8 (Name),
              Form => UTF8_File_Form);
   end Wide_Create;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Create (File,
              Mode => Out_File,
              Name => Wide_To_UTF8 (Name),
              Form => UTF8_File_Form);
   end Wide_Create;

   -----------------------
   -- Wide_Is_Directory --
   -----------------------

   function Wide_Is_Directory (Name : Wide_String) return Boolean is
   begin
      return Wide_Exists (Name)
         and then Kind (Wide_To_UTF8 (Name)) = Directory;
   end Wide_Is_Directory;

   -----------------------------
   -- Wide_Is_Executable_File --
   -----------------------------

   function Wide_Is_Executable_File (Name : Wide_String) return Boolean is
   begin
      return Wide_Is_File (Name)
             and then GNAT.OS_Lib.Is_Executable_File (Wide_To_UTF8 (Name));
   end Wide_Is_Executable_File;

   ------------------
   -- Wide_Is_File --
   ------------------

   function Wide_Is_File (Name : Wide_String) return Boolean is
   begin
      return Wide_Exists (Name)
         and then Kind (Wide_To_UTF8 (Name)) = Ordinary_File;
   end Wide_Is_File;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Text_IO.File_Type;
                        Mode : Ada.Text_IO.File_Mode;
                        Name : Wide_String) is
      use Ada.Text_IO;
   begin
      Open (File,
            Mode => Mode,
            Name => Wide_To_UTF8 (Name),
            Form => UTF8_File_Form);
   end Wide_Open;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : Ada.Wide_Text_IO.File_Mode;
                        Name : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Open (File,
            Mode => Mode,
            Name => Wide_To_UTF8 (Name),
            Form => UTF8_File_Form);
   end Wide_Open;

end ZanyBlue.OS;
