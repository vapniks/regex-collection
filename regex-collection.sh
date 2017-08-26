# Written by Joe Bloggs [2017-08-26 Sat]

# This file contains a collection of handy regular expressions assigned to variables.
# It should be sourced into zsh to work properly.
# It can be useful for awk & sed scripts for example, either sourced in a script or on the command line.
# Each variable name is prefixed with either BRE_, ERE_, or PCRE_ to indicate the type of regular expression.
# (BRE = basic regular expression, ERE = extended regular expression, PCRE = perl compatible regular expression)

# Note: grep supports BRE, ERE & PCRE, whereas awk & sed only support BRE & ERE however you can use the programming
# features of awk & sed to get the same results as PCRE. You can also run perl in sed mode with "perl -pe" which
# allows you to use sed command syntax but with perl regexps
# To learn more about PCRE see the perlre manpage.

# Each regular expression is parenthesized, but may include useful subgroups. See the comments before each regexp.

# You can find more regular expressions here: http://regexlib.com/

# Notes: if parsing text files containing strange non-ascii chars some of these regexps might not match as intended.
#        You can convert to ascii using "iconv -t ASCII//IGNORE" or "iconv -t ASCII//TRANSLIT" on the command line.
#
#        You can test regexps with the testregexp function defined in another file
#        (zsh doesn't allow exporting functions from within this file)
#        Each regexp defined below has associated tests written underneath.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.
# If not, see <http://www.gnu.org/licenses/>.

source "$(dirname $0)/testregexp"

# general purpose csv field, either quoted or unquoted, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
# e.g. find lines of data.csv where No. of csv fields isn't 10: grep -v -x -P "^$PCRE_CSVFIELD{10}$" data.csv
ERE_CSVFIELD="(\"[^\"]*\"|'[^']*'|[^\"'][^,]*)"
echo testing ERE_CSVFIELD
testregexp "^${ERE_CSVFIELD}$" "'1,2,3'" "fsad" "\"foo\"" "\"as'cd'df\""
testregexp -n "^${ERE_CSVFIELD}$" "1,2,3" "\"a\"b\"" 
PCRE_CSVFIELD="((?:^|,)\K(?:\"[^\"]*\"|\'[^\']*\'|[^,]*)(?=(?:,|$)))"
echo testing PCRE_CSVFIELD
testregexp -p "^${PCRE_CSVFIELD}$" "'1,2,3'" "fsad" "\"foo\"" "\"as'cd'df\""
testregexp -p -n "^${PCRE_CSVFIELD}$" "1,2,3" "\"a\"b\"" 
# quoted general purpose csv field, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
ERE_QUOTEDCSVFIELD="(\"[^\"]*\"|\'[^\']*\')"
PCRE_QUOTEDCSVFIELD="((?:^|,)\K(?:\"[^\"]*\"|\'[^\']*\')(?=(?:,|$)))"
# unquoted general purpose csv field, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
ERE_UNQUOTEDCSVFIELD="([^\"'][^,]*|)"
PCRE_UNQUOTEDCSVFIELD="((?:^|,)\K(?:[^,\"\']*)(?=(?:,|$)))"
# general purpose csv field, either quoted or unquoted, including delimiter (assumes 1st field is non-empty)
# e.g. find lines of data.csv where No. of csv fields isn't 10: grep -v -x -E "^$ERE_CSVFIELDWITHDELIM{10}$" data.csv
ERE_CSVFIELDWITHDELIM="(^(?:\"[^\"]*\"|\'[^\']*\'|[^,]+)|,(:?\"[^\"]*\"|\'[^\']*\'|[^,]*))"
# datestamp in the form DD/MM/YYYY (contains 1 subgroup)
ERE_DATE="([0-9]{2}/[0-9]{2}/(19|20)[0-9]{2})"
# datestamp in the form DD/MM/YYYY HH:MM (contains 1 subgroup)
ERE_DATETIME="([0-9]{2}/[0-9]{2}/(19|20)[0-9]{2} [012][0-9]:[012345][0-9])"
# timestamp in the form HH:MM:SS
ERE_TIMEHMS="([012][0-9]:[012345][0-9]:[012345][0-9])"
# timestamp in the form HH:MM
ERE_TIMEHM="([012][0-9]:[012345][0-9])"
# hour of day (1 or 2 digits)
ERE_HOUR="([012]?[0-9])"
# minute of day (1 or 2 digits)
ERE_MINUTE="([012345]?[0-9])"
# part of day, morning, afternoon, evening or night
ERE_DAYPART="([Mm]orning|[Aa]fternoon|[Ee]vening|[Nn]ight)"
PCRE_DAYPART="((?i)morning|afternoon|evening|night)"
# weekday
ERE_WEEKDAY="([Mm]onday|MONDAY|[Tt]uesday|TUESDAY|[Ww]ednesday|WEDNESDAY|[Tt]hursday|THURSDAY|[Ff]riday|FRIDAY)"
PCRE_WEEKDAY="((?i)?mon(:?day)?|tue(:?sday)?|wed(:?nesday)?|thurs(:?day)?|fri(:?day)?|sat(:?urday)?|sun(:?day)?)"
# month
ERE_MONTH="([Jj]anuary)|JANUARY|[Ff]ebruary|FEBRUARY|[Mm]arch|MARCH|[Aa]pril|APRIL|[Mm]ay|MAY|[Jj]une|JUNE|[Jj]uly|JULY|[Aa]ugust|AUGUST|[Ss]eptember|SEPTEMBER|[Oo]ctober|OCTOBER|[Nn]ovember|NOVEMBER)"
PCRE_MONTH="((?i)jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sept(?:ember)?|oct(:?ober)?|nov(?:ember)?|dec(:?ember)?)"
# year between 1900 and 2099
ERE_YEAR="(19[0-9]{2}|20[0-9]{2})"
# database table storage row key with 18 digits
ERE_DATABASEROWKEY="([0-9]{18})"
# arbitrary size decimal number
ERE_NUMBER="([0-9]+\.?[0-9]*)"
# arbitrary size integer
ERE_INTEGER="([0-9]+)"
# letters only (no punctuation or whitespace)
ERE_LETTERS="([[:alpha]]+)"
# letters and numbers (no punctuation or whitespace)
ERE_ALNUM="([[:alnum:]]+)"
# UK postcode
ERE_UKPOSTCODE="([a-zA-Z]{1,2}[0-9][0-9A-Za-z]{0,1} ?[0-9]?[A-Za-z]{2})"
# ONS/GSS Output Area code (see here: https://en.wikipedia.org/wiki/ONS_coding_system)
ERE_ONS_CODE="(E|J|K|L|M|N|S|W|)[0-9]{8}"
ERE_ONS_OA="E00[0-9]{6}"
ERE_ONS_LSOA="(E|W)01[0-9]{6}"
ERE_ONS_SOA="(E|W)0(0|1|2|3)[0-9]{6}"
# NUTS (Nomenclature of Territorial Units for Statistics) area codes
ERE_NUTS="[A-Z][A-Z][1-9A-Z]{1,3}"
ERE_NUTS1="[A-Z][A-Z][1-9A-Z]"
ERE_NUTS2="[A-Z][A-Z][1-9A-Z][1-9A-Z]"
ERE_NUTS3="[A-Z][A-Z][1-9A-Z][1-9A-Z][0-9A-Z]"
# FIPS (Federal Information Processing Standards) codes
ERE_FIPS104="[A-Z][A-Z][0-9][0-9]"
# email address
ERE_EMAIL="(\w[[:alnum:]._-]*\w@\w[[:alnum:].-]*\w\.\w{2,3})"
# UK phone number (contains 1 subgroup - the area code)
ERE_UKPHONE="(\s*\(?0[0-9]{3,5}\)?\s*[0-9]{3,4}\s*[0-9]{3,4}\s*)"
# international phone number: optional country code followed by area code surrounded with '-' or '(' and ')',
# or just an area code optionally starting with 0, followed by phone numder. The number itself may contain spaces and '-'
# (contains 2 subgroups)
ERE_INTLPHONE="((\+[1-9][0-9]*(\([0-9]*\)|-[0-9]*-))?[0]?[1-9][0-9\ -]*)"
# 3-14 char password starting with a letter 
ERE_PASSWORD="([a-zA-Z]\w{3,14})"
# Network port (0-65535)
ERE_PORT="([0-9]{1,4}|[0-6][0-9]{4})"
ERE_PORT_EXACT="([0-9]{1,4}|[1-5][0-9]{4})"
ERE_PORT_EXACT="([1-9][0-9]{0,3}|[1-5][0-9]{4}|6([0-4][0-9]{3}|5([0-4][0-9]{2}|5[0-2][0-9]|53[0-5])))"
# Well-known/system port (1-1023)
ERE_SYSTEM_PORT="([0-9]{3}|10[0-9]{2})"
# Registered port (1024-49151)
ERE_REGISTERED_PORT="([0-9]{4}|[0-4][0-9]{4})"
# Dynamic/private port (49152-65535)
ERE_DYNAMIC_PORT="([4-6][0-9]{4})"
# Network subnet
ERE_SUBNET_PREFIX_LEN="([0-9]|[0-2][0-9]|3[0-2])"
# MAC address (contains 2 subgroups)
ERE_MAC="(([0-9a-fA-F][0-9a-fA-F]:){5}([0-9a-fA-F][0-9a-fA-F]))"
# IPv4 address with optional port or subnet (contains 3 subgroups - 1st octet, final 3 octets & optional :port/subnet)
ERE_IPV4="((25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})(\.(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})){3}(:${ERE_PORT}|/${ERE_SUBNET_PREFIX_LEN})?)"
# IPv6 address (compressed or uncompressed) with optional port or subnet
# (contains 3 subgroups - 1st octet, final 3 octets & optional :port/subnet)
ERE_IPV6="(([0-9a-fA-F]{0,4}:){2,7}(:|[0-9a-fA-F]{1,4})(:${ERE_PORT}|/${ERE_SUBNET_PREFIX_LEN})?)"
# Version 3/4 UUID 
ERE_UUID="([A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[34][A-Fa-f0-9]{3}-[89ab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12})"
# domain names
ERE_DOMAIN_NAME="([a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,})"
# Any valid http/https/ftp URL (contains 3 subgroups - the protocol, domain name, and optional query at the end of the URL)
ERE_URL="((https?|ftp)://${ERE_DOMAIN_NAME}(/\S*)?)"
# An FTP URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
ERE_FTP="(ftp://${ERE_DOMAIN_NAME}(/\S*)?)"
# An HTTP URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
ERE_HTTP="(http://${ERE_DOMAIN_NAME}(/\S*)?)"
# An HTTPS URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
ERE_HTTPS="(https://${ERE_DOMAIN_NAME}(/\S*)?)"
# Output from 'ls -l', with the following subgroups:
# \1 = file permissions, \2 = number of files, \3 = GID, \4 = UID, \5 = size, \6 = date, \7 = filename
ERE_LL="([rwdx-]{10})\s+([0-9]+)\s+([a-z]+) ([a-z]+)\s+([0-9.KMG]+) ([a-zA-Z]+\s+[0-9]+\s+[0-9:]+) (.+)$"
