/* cantor
 * Author: claude
 * Creation date: 10/13/23
 */
MACHINE
    cantor

CONSTANTS
    xx

PROPERTIES
    xx : INTEGER

ASSERTIONS
    ! ff. (ff : INTEGER --> POW(INTEGER) => # pp. (! nn. (pp : POW(INTEGER) & nn : INTEGER => ff(nn) /= pp)))
END