--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client.Time_Zones              Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  20:46 27 Aug 2020  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

package GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
        Time_Zones is
   --
   -- North hemisphere
   --
          -- Standard time
   SST   : constant Zone_Data := (3, 10, Su, 3,-11.0 * 3600.0, "SST" );
   HAST  : constant Zone_Data := (4, 10, Su, 3,-10.0 * 3600.0, "HAST");
   AKST  : constant Zone_Data := (4, 10, Su, 3, -9.0 * 3600.0, "AKST");
   PST   : constant Zone_Data := (3, 10, Su, 3, -8.0 * 3600.0, "PST" );
   MST   : constant Zone_Data := (3, 10, Su, 3, -7.0 * 3600.0, "MST" );
   CST   : constant Zone_Data := (3, 10, Su, 3, -6.0 * 3600.0, "CST" );
   EST   : constant Zone_Data := (3, 10, Su, 3, -5.0 * 3600.0, "EST" );
   AST   : constant Zone_Data := (3, 10, Su, 3, -4.0 * 3600.0, "AST" );
   WGT   : constant Zone_Data := (3, 10, Su, 3, -3.0 * 3600.0, "WGT" );
   EGT   : constant Zone_Data := (3, 10, Su, 3, -1.0 * 3600.0, "EGT" );
   MET   : constant Zone_Data := (3, 10, Su, 3,  1.0 * 3600.0, "MET" );
   EET   : constant Zone_Data := (3, 10, Su, 3,  2.0 * 3600.0, "EET" );
   FET   : constant Zone_Data := (3, 10, Su, 3,  3.0 * 3600.0, "FET" );
   MSK   : constant Zone_Data := (3, 10, Su, 3,  3.0 * 3600.0, "MSK" );
   SAMT  : constant Zone_Data := (4, 10, Su, 3,  4.0 * 3600.0, "SAMT");
   YEKT  : constant Zone_Data := (4, 10, Su, 3,  5.0 * 3600.0, "YEKT");
   OMST  : constant Zone_Data := (4, 10, Su, 3,  6.0 * 3600.0, "OMST");
   KRAT  : constant Zone_Data := (4, 10, Su, 3,  7.0 * 3600.0, "KRAT");
   IRKT  : constant Zone_Data := (4, 10, Su, 3,  8.0 * 3600.0, "IRKT");
   YAKT  : constant Zone_Data := (4, 10, Su, 3,  9.0 * 3600.0, "YAKT");
   VLAT  : constant Zone_Data := (4, 10, Su, 3, 10.0 * 3600.0, "VLAT");
   MAGT  : constant Zone_Data := (4, 10, Su, 3, 11.0 * 3600.0, "MAGT");
   PETT  : constant Zone_Data := (4, 10, Su, 3, 12.0 * 3600.0, "PETT");

          -- Daylight saving time
   SDT   : constant Zone_Data := (3, 3, Su, 2, -10.0 * 3600.0, "SDT"  );
   HADT  : constant Zone_Data := (4, 3, Su, 2,  -9.0 * 3600.0, "HADT" );
   AKDT  : constant Zone_Data := (4, 3, Su, 2,  -8.0 * 3600.0, "AKDT" );
   PDT   : constant Zone_Data := (3, 3, Su, 2,  -7.0 * 3600.0, "PDT"  );
   MDT   : constant Zone_Data := (3, 3, Su, 2,  -6.0 * 3600.0, "MDT"  );
   CDT   : constant Zone_Data := (3, 3, Su, 2,  -5.0 * 3600.0, "CDT"  );
   EDT   : constant Zone_Data := (3, 3, Su, 2,  -4.0 * 3600.0, "EDT"  );
   WGST  : constant Zone_Data := (4, 3, Su, 2,  -2.0 * 3600.0, "WGST" );
   EGST  : constant Zone_Data := (4, 3, Su, 2,   0.0 * 3600.0, "EGST" );
   BST   : constant Zone_Data := (3, 3, Su, 2,   1.0 * 3600.0, "BST"  );
   EEST  : constant Zone_Data := (4, 3, Su, 2,   3.0 * 3600.0, "EEST" );
   FEST  : constant Zone_Data := (4, 3, Su, 2,   4.0 * 3600.0, "FEST" );
-- MSD   : constant Zone_Data := (3, 3, Su, 2,   4.0 * 3600.0, "MSD"  );
   CLST  : constant Zone_Data := (4, 3, Su, 2,   5.0 * 3600.0, "CLST" );
-- YEKST : constant Zone_Data := (5, 3, Su, 2,   6.0 * 3600.0, "YEKST");
-- OMSST : constant Zone_Data := (5, 3, Su, 2,   7.0 * 3600.0, "OMSST");
-- KRAST : constant Zone_Data := (5, 3, Su, 2,   8.0 * 3600.0, "KRAST");
-- IRKST : constant Zone_Data := (5, 3, Su, 2,   9.0 * 3600.0, "IRKST");
-- YAKST : constant Zone_Data := (5, 3, Su, 2,  10.0 * 3600.0, "YAKST");
-- VLAST : constant Zone_Data := (5, 3, Su, 2,  11.0 * 3600.0, "VLAST");
-- MAGST : constant Zone_Data := (5, 3, Su, 2,  12.0 * 3600.0, "MAGST");
-- PETST : constant Zone_Data := (5, 3, Su, 2,  13.0 * 3600.0, "PETST");
   --
   -- South hemisphere
   --
          -- Daylight saving time
   EASST : constant Zone_Data := (5, 10, Su, 3, -5.0 * 3600.0, "EASST");
   SAMST : constant Zone_Data := (5, 10, Su, 3, -3.0 * 3600.0, "SAMST");
   BRST  : constant Zone_Data := (4, 10, Su, 3, -2.0 * 3600.0, "BRST" );
   AWDT  : constant Zone_Data := (4, 10, Su, 3,  9.0 * 3600.0, "AWDT" );
   AEDT  : constant Zone_Data := (4, 10, Su, 3, 11.0 * 3600.0, "AEDT" );
   NZDT  : constant Zone_Data := (4, 10, Su, 3, 13.0 * 3600.0, "NZDT" );

          -- Standard time
   EAST  : constant Zone_Data := (4, 3, Su, 3,  -6.0 * 3600.0, "EAST" );
   COT   : constant Zone_Data := (3, 3, Su, 3,  -5.0 * 3600.0, "COT"  );
   CLT   : constant Zone_Data := (3, 3, Su, 3,  -4.0 * 3600.0, "CLT"  );
   BRT   : constant Zone_Data := (3, 3, Su, 3,  -3.0 * 3600.0, "BRT"  );
   AWST  : constant Zone_Data := (4, 3, Su, 2,   8.0 * 3600.0, "AWST" );
   AEST  : constant Zone_Data := (4, 3, Su, 2,  10.0 * 3600.0, "AEST" );
   NZST  : constant Zone_Data := (4, 3, Su, 2,  12.0 * 3600.0, "NZST" );

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
    Time_Zones;
