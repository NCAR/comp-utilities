file(GLOB PRO_FILES "*.pro")
install(FILES ${PRO_FILES} DESTINATION src)

file(GLOB PNG_FILES "*.png")
install(FILES ${PNG_FILES} DESTINATION src)

file(GLOB SAV_FILES "*.sav")
install(FILES ${SAV_FILES} DESTINATION src)

file(GLOB LUT_FILES "*.lut")
install(FILES ${LUT_FILES} DESTINATION src)
