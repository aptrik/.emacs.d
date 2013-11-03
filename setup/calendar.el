(eval-after-load "calendar"
  '(progn
     (european-calendar)))

(setq calendar-location-name           "Göteborg, SE"
      calendar-latitude                57.72 ; 57° 43' North
      calendar-longitude               11.97 ; 11° 58' East
      calendar-week-start-day          1
      calendar-offset                  0
      calendar-today-marker            'calendar-today-face
      view-calendar-holidays-initially nil
      calendar-date-display-form       '((format "%s-%02d-%02d"
                                                 year
                                                 (string-to-number month)
                                                 (string-to-number day)))
      view-diary-entries-initially     t
      mark-holidays-in-calendar        t
      mark-diary-entries-in-calendar   t
      calendar-time-display-form       '(24-hours ":" minutes)
      diary-display-hook               'fancy-diary-display
      holidays-in-diary-buffer         t
      diary-list-include-blanks        t
      diary-file                       (expand-file-name "~/.diary"))

(setq calendar-day-name-array
      ["Söndag" "Måndag" "Tisdag" "Onsdag" "Torsdag" "Fredag" "Lördag"]
      calendar-month-name-array
      ["Januari" "Februari" "Mars" "April" "Maj" "Juni"
       "Juli" "Augusti" "September" "Oktober" "November" "December"])

(add-hook 'diary-display-hook          'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; (add-hook 'diary-hook                  'appt-make-list)
(add-hook 'list-diary-entries-hook     'sort-diary-entries t)

(setq all-christian-calendar-holidays nil
      general-holidays                nil
      oriental-holidays               nil
      hebrew-holidays                 nil
      islamic-holidays                nil)

(setq swedish-holidays
      ;; Se: http://www.kalender.se
      ;; och: http://hem.passagen.se/farila/holiday.htm
      '((holiday-fixed  1  1    "Nyårsdagen")
        (holiday-fixed  1  6    "Trettondedag jul")
        (holiday-fixed  2 14    "Alla hjärtans dag")
        (holiday-fixed  4 30    "Valborgmässoafton")
        (holiday-fixed  5  1    "Första maj/Valborg")
        (holiday-float  5  0 -1 "Mors dag") ; Sista söndagen i maj
        (holiday-fixed  6  6    "Sveriges nationaldag (svenska flaggans dag)")
        ;; Midsommardagen 2005-06-25
        (holiday-float 11  6  1 "Alla helgons dag") ; Första lördagen i nov.
        (holiday-float 11  0  2 "Fars dag") ; Andra söndagen i november
        (holiday-fixed 12 10    "Nobeldagen")
        (holiday-fixed 12 25    "Juldagen")
        (holiday-fixed 12 26    "Annandag jul")

        (holiday-advent 0 "Första advent")

        (holiday-easter-etc -2 "Långfredag")
        (holiday-easter-etc 0  "Påskdagen")
        (holiday-easter-etc 1  "Annandag påsk")
        (holiday-easter-etc 39 "Kristi himmelsfärds dag")
        (holiday-easter-etc 49 "Pingstdagen")
        (holiday-easter-etc 50 "Annandag pingst")
        ))

(setq calendar-holidays
      (append
       general-holidays
       local-holidays
       other-holidays
       christian-holidays
       solar-holidays))

(setq calendar-holidays
      (append
       swedish-holidays))
