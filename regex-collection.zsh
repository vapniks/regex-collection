# Written by Joe Bloggs [2017-08-26 Sat]

# This file contains a collection of handy regular expressions assigned to arrays.
# It should be sourced into zsh to work properly.
# It can be useful for awk & sed scripts for example, either sourced in a script or on the command line.
#
# Basic regular expressions are stored in the RC_BRE array, extended regular expressions are stored in RC_ERE,
# and perl compatible regular expressions are stored in RC_PCRE.
# The array keys describe their associated regular expression values, e.g. RC_ERE[csvfield]
# (use tab completion after [ to see all the keys).

# Note: grep supports BRE, ERE & PCRE, whereas awk & sed only support BRE & ERE however you can use the programming
# features of awk & sed to get the same results as PCRE. You can also run perl in sed mode with "perl -pe" which
# allows you to use sed command syntax but with perl regexps
# To learn more about PCRE see the perlre manpage.

# Each regular expression is parenthesized, but may include useful subgroups. See the comments before each regexp.

# You can find more regular expressions here: http://regexlib.com/

# For testing purposes make sure the testregexp function is defined or is available in the same directory as this
# file, and source this file after setting TESTREGEXP to a non-empty string, like this:
# > TESTREGEXP=t source regex-collection.sh

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

# TODO: finish writing tests and fix regexps

if [[ -r "$(dirname $0)/testregexp" ]]; then
    source "$(dirname $0)/testregexp"
fi

typeset -A RC_BRE RC_ERE RC_PCRE
# quoted general purpose csv field, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
RC_ERE[quotedcsvfield]="(?:\"[^\"]*\"|\'[^\']*\')"
RC_PCRE[quotedcsvfield]="(?:(?:^|,)\K(?:\"[^\"]*\"|\'[^\']*\')(?=(?:,|$)))"
# unquoted general purpose csv field, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
RC_ERE[unquotedcsvfield]="(?:[^\"'][^,]*|)"
RC_PCRE[unquotedcsvfield]="(?:(?:^|,)\K(?:[^,\"\'][^,]*|)(?=(?:,|$)))"
# general purpose csv field, either quoted or unquoted, not including delimiters
# ERE version doesn't check delimiters, PCRE version does
# e.g. find lines of data.csv where No. of csv fields isn't 10: grep -v -x -P "^${RC_PCRE[csvfield]}{10}$" data.csv
# or: grep -v -x -E "^${RC_ERE[csvfieldwithdelim]}{10}$" data.csv
RC_ERE[csvfield]="(${RC_ERE[quotedcsvfield]}|${RC_ERE[unquotedcsvfield]})"
RC_ERE[csvfieldwithdelim]="((?:^|,)${RC_ERE[csvfield]})"
RC_PCRE[csvfield]="(${RC_PCRE[quotedcsvfield]}|${RC_PCRE[unquotedcsvfield]})"
if [[ -n ${TESTREGEXP} ]]; then
    echo testing CSVFIELD regexps
    testregexp "^${RC_ERE[csvfield]}$" "'1,2,3'" "fsad" "\"foo\"" "\"as'cd'df\"" ""
    testregexp -n "^${RC_ERE[csvfield]}$" "1,2,3" "\"a\"b\"" "'as'df'"
    testregexp -p "^${RC_PCRE[csvfield]}$" "'1,2,3'" "fsad" "\"foo\"" "\"as'cd'df\"" ""
    testregexp -p -n "^${RC_PCRE[csvfield]}$" "1,2,3" "\"a\"b\"" "'as'df'"
fi
# datestamp in the form DD/MM/YYYY (contains 1 subgroup)
RC_ERE[date]="([0-9]{2}\/[0-9]{2}\/(19|20)[0-9]{2})"
# datestamp in the form DD/MM/YYYY HH:MM (contains 1 subgroup)
RC_ERE[datetime]="([0-9]{2}\/[0-9]{2}\/(19|20)[0-9]{2} [012][0-9]:[012345][0-9])"
# timestamp in the form HH:MM:SS
RC_ERE[timehms]="([012][0-9]:[012345][0-9]:[012345][0-9])"
# timestamp in the form HH:MM
RC_ERE[timehm]="([012][0-9]:[012345][0-9])"
# hour of day (1 or 2 digits)
RC_ERE[hour]="([012]?[0-9])"
# minute of day (1 or 2 digits)
RC_ERE[minute]="([012345]?[0-9])"
# part of day, morning, afternoon, evening or night
RC_ERE[daypart]="([Mm]orning|[Aa]fternoon|[Ee]vening|[Nn]ight)"
RC_PCRE[daypart]="((?i)morning|afternoon|evening|night)"
# weekday
RC_ERE[weekday]="([Mm]onday|MONDAY|[Tt]uesday|TUESDAY|[Ww]ednesday|WEDNESDAY|[Tt]hursday|THURSDAY|[Ff]riday|FRIDAY)"
RC_PCRE[weekday]="((?i)?mon(:?day)?|tue(:?sday)?|wed(:?nesday)?|thurs(:?day)?|fri(:?day)?|sat(:?urday)?|sun(:?day)?)"
# month
RC_ERE[month]="([Jj]anuary)|JANUARY|[Ff]ebruary|FEBRUARY|[Mm]arch|MARCH|[Aa]pril|APRIL|[Mm]ay|MAY|[Jj]une|JUNE|[Jj]uly|JULY|[Aa]ugust|AUGUST|[Ss]eptember|SEPTEMBER|[Oo]ctober|OCTOBER|[Nn]ovember|NOVEMBER)"
RC_PCRE[month]="((?i)jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sept(?:ember)?|oct(:?ober)?|nov(?:ember)?|dec(:?ember)?)"
# year between 1900 and 2099
RC_ERE[year]="(19[0-9]{2}|20[0-9]{2})"
# database table storage row key with 18 digits
RC_ERE[databaserowkey]="([0-9]{18})"
# arbitrary size decimal number
RC_ERE[number]="([0-9]+\.?[0-9]*)"
# arbitrary size integer
RC_ERE[integer]="([0-9]+)"
# letters only (no punctuation or whitespace)
RC_ERE[letters]="([[:alpha]]+)"
# letters and numbers (no punctuation or whitespace)
RC_ERE[alnum]="([[:alnum:]]+)"
# UK postcode
RC_ERE[ukpostcode]="([a-zA-Z]{1,2}[0-9][0-9A-Za-z]{0,1} ?[0-9]?[A-Za-z]{2})"
# ONS/GSS Output Area code (see here: https://en.wikipedia.org/wiki/ONS_coding_system)
RC_ERE[ons_code]="(E|J|K|L|M|N|S|W|)[0-9]{8}"
RC_ERE[ons_oa]="E00[0-9]{6}"
RC_ERE[ons_lsoa]="(E|W)01[0-9]{6}"
RC_ERE[ons_soa]="(E|W)0(0|1|2|3)[0-9]{6}"
# NUTS (Nomenclature of Territorial Units for Statistics) area codes
RC_ERE[nuts]="[A-Z][A-Z][1-9A-Z]{1,3}"
RC_ERE[nuts1]="[A-Z][A-Z][1-9A-Z]"
RC_ERE[nuts2]="[A-Z][A-Z][1-9A-Z][1-9A-Z]"
RC_ERE[nuts3]="[A-Z][A-Z][1-9A-Z][1-9A-Z][0-9A-Z]"
# FIPS (Federal Information Processing Standards) codes
RC_ERE[fips104]="[A-Z][A-Z][0-9][0-9]"
# email address
RC_ERE[email]="(\w[[:alnum:]._-]*\w@\w[[:alnum:].-]*\w\.\w{2,3})"
# UK phone number (contains 1 subgroup - the area code)
RC_ERE[ukphone]="(\s*\(?0[0-9]{3,5}\)?\s*[0-9]{3,4}\s*[0-9]{3,4}\s*)"
# international phone number: optional country code followed by area code surrounded with '-' or '(' and ')',
# or just an area code optionally starting with 0, followed by phone numder. The number itself may contain spaces and '-'
# (contains 2 subgroups)
RC_ERE[intlphone]="((\+[1-9][0-9]*(\([0-9]*\)|-[0-9]*-))?[0]?[1-9][0-9\ -]*)"
# 3-14 char password starting with a letter 
RC_ERE[password]="([a-zA-Z]\w{3,14})"
# Network port (0-65535)
RC_ERE[port]="([0-9]{1,4}|[0-6][0-9]{4})"
RC_ERE[port_exact]="([0-9]{1,4}|[1-5][0-9]{4})"
RC_ERE[port_exact]="([1-9][0-9]{0,3}|[1-5][0-9]{4}|6([0-4][0-9]{3}|5([0-4][0-9]{2}|5[0-2][0-9]|53[0-5])))"
# Well-known/system port (1-1023)
RC_ERE[system_port]="([0-9]{3}|10[0-9]{2})"
# Registered port (1024-49151)
RC_ERE[registered_port]="([0-9]{4}|[0-4][0-9]{4})"
# Dynamic/private port (49152-65535)
RC_ERE[dynamic_port]="([4-6][0-9]{4})"
# Network subnet
RC_ERE[subnet_prefix_len]="([0-9]|[0-2][0-9]|3[0-2])"
# MAC address (contains 2 subgroups)
RC_ERE[mac]="(([0-9a-fA-F][0-9a-fA-F]:){5}([0-9a-fA-F][0-9a-fA-F]))"
# IPv4 address with optional port or subnet (contains 3 subgroups - 1st octet, final 3 octets & optional :port/subnet)
RC_ERE[ipv4]="((25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})(\.(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})){3}(:${RC_ERE[port]}|\/${RC_ERE[subnet_prefix_len]})?)"
# IPv6 address (compressed or uncompressed) with optional port or subnet
# (contains 3 subgroups - 1st octet, final 3 octets & optional :port/subnet)
RC_ERE[ipv6]="(([0-9a-fA-F]{0,4}:){2,7}(:|[0-9a-fA-F]{1,4})(:${RC_ERE[port]}|\/${RC_ERE[subnet_prefix_len]})?)"
# Version 3/4 UUID 
RC_ERE[uuid]="([A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[34][A-Fa-f0-9]{3}-[89ab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12})"
# domain names
RC_ERE[domain_name]="([a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,})"
# Any valid http/https/ftp URL (contains 3 subgroups - the protocol, domain name, and optional query at the end of the URL)
RC_ERE[url]="((https?|ftp):\/\/${RC_ERE[domain_name]}(\/\S*)?)"
# An FTP URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
RC_ERE[ftp]="(ftp:\/\/${RC_ERE[domain_name]}(\/\S*)?)"
# An HTTP URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
RC_ERE[http]="(http:\/\/${RC_ERE[domain_name]}(\/\S*)?)"
# An HTTPS URL (contains 2 subgroups - the domain name and optional query at the end of the URL)
RC_ERE[https]="(https:\/\/${RC_ERE[domain_name]}(\/\S*)?)"
# Output from 'ls -l', with the following subgroups:
# \1 = file permissions, \2 = number of files, \3 = GID, \4 = UID, \5 = size, \6 = date, \7 = filename
RC_ERE[ll]="([rwdx-]{10})\s+([0-9]+)\s+([a-z]+) ([a-z]+)\s+([0-9.KMG]+) ([a-zA-Z]+\s+[0-9]+\s+[0-9:]+) (.+)$"
