MODULE TYPEDEF
    IMPLICIT NONE
	TYPE :: time_info
        INTEGER year
        INTEGER mon
        INTEGER day
        INTEGER hr
    END TYPE time_info

    TYPE :: airport_info
        REAL :: lon ,lat,dis
        INTEGER :: sta,num
        TYPE(time_info) :: time
        INTEGER ::  vis,clev
        INTEGER ::  total_cld,low_cld
    END TYPE airport_info

    TYPE :: station_info
        INTEGER :: num
        REAL    :: lat,lon
    END TYPE station_info

    TYPE :: cloud_info
        INTEGER :: sta
        REAL :: lon ,lat ,slev
        TYPE(time_info) :: time
        INTEGER :: vis,clev,total_cld,low_cld
    END TYPE cloud_info

END MODULE

MODULE QsortC_Module
  implicit none
  public :: QsortC
  private :: Partition

  contains

  recursive subroutine QsortC(A,D)
    real, intent(in out), dimension(:) :: A
    integer,intent(in out),dimension(:) :: D
    integer :: iq
    if(size(A) > 1) then
      call Partition(A,D,iq)
      call QsortC(A(:iq-1),D(:iq-1))
      call QsortC(A(iq:),D(iq:))
    endif
  end subroutine QsortC

  subroutine Partition(A,D,marker)
    real, intent(in out), dimension(:) :: A
    integer,intent(in out),dimension(:) :: D
    integer, intent(out) :: marker
    integer :: i, j
    real :: temp
    real :: x
    integer :: cache
    cache = D(1)
    x = A(1)
    i= 0
    j= size(A) + 1
    do
      j = j-1
      do
        if (A(j) <= x) exit
        j = j-1
      end do
      i = i+1
      do
        if (A(i) >= x) exit
        i = i+1
      end do
      if (i < j) then
        temp = A(i)
        A(i) = A(j)
        A(j) = temp

	cache = D(i)
	D(i) = D(j)
	D(j) = cache

      elseif (i == j) then
        marker = i+1
        return
      else
        marker = i
        return
      endif
    end do
  end subroutine Partition

    END MODULE  QsortC_Module

PROGRAM HX002
    USE TYPEDEF
    USE QsortC_Module
    IMPLICIT NONE
    INTEGER,PARAMETER::NA = 354,NS = 2421,NC = 2421
    REAL,PARAMETER :: R = 1.0
    EXTERNAL BubbleSort,swap
    INTEGER,EXTERNAL :: getfilelines
    CHARACTER*8,EXTERNAL :: lastday

    TYPE(airport_info) :: air(NA)
    TYPE(station_info) :: sta(NS)
    TYPE(cloud_info)   :: cld(NC)
    type(time_info)    :: stime

    INTEGER i,j,k,date_time(8)
	LOGICAL :: alive
    REAL :: dis(NS),dist(NA,NS)
    INTEGER :: num(NS),numt(NA,NS)
    CHARACTER*2 :: hr
    CHARACTER*10  :: b(3)
    CHARACTER*100 :: FILE_NAME,PATH,FILE_PATH_IN,FILE_PATH_OUT
    CHARACTER*8 :: TIME,LASTIME
!########################################################
    !read system time
    call date_and_time(b(1),b(2),b(3),date_time)
    !stime%year = date_time(1)
    !stime%mon  = date_time(2)
    !stime%day  = date_time(3)
    stime%hr   =  date_time(5)
    write(hr,'(i2.2)') stime%hr

    ! READ DATE INFO
    OPEN(50,file="filedate.txt")
    READ(50,'(BN,a50)') TIME
    PRINT*,"READ TIME:", TRIM(TIME)//hr," OK!"
    CLOSE(50)

    read(time(1:4),'(i4.4)') stime%year
    read(time(5:6),'(i2.2)') stime%mon
    read(time(7:8),'(i2.2)') stime%day

    LASTIME = lastday(stime%year,stime%mon,stime%day)

    ! READ PATH INFO
    OPEN(40,file="path.txt")
    READ(40,'(BN,a50)') PATH
    PRINT*,"READ PATH:", TRIM(PATH)," OK!"
    CLOSE(40)

    ! DEFINE IN&OUT PATH
    FILE_PATH_IN  = TRIM(PATH)//'/input/cloud/'
    FILE_PATH_OUT = TRIM(PATH)//'/output/surface/present/'

    ! DEFINE FIEL_NAME FIXED PART
    FILE_NAME = "HX_P_CLOUD"

    ! READ AIRPORT INFO
    OPEN(10,file= TRIM(PATH)//"/input/airport-20160712.txt")
    READ(10,*) (air(i)%num,air(i)%lon,air(i)%lat,i=1,NA)
    CLOSE(10)
    print*,"READ AIRPORT INFO OK!"

    ! READ STATION INFO
    OPEN(60,file= TRIM(PATH)//"/input/StationInfo.txt")
    READ(60,*) (sta(i)%num,sta(i)%lat,sta(i)%lon,i=1,NS)
    CLOSE(60)
    PRINT*,"READ STATION INFO OK!"

    ! SORT FOR THE STATION NEAR EVERY AIRPORT
    DO i = 1,NA
        DO j = 1, NS
            dis(j) = SQRT((sta(j)%lat-air(i)%lat)**2+(sta(j)%lon-air(i)%lon)**2)
            num(j) = sta(j)%num
        ENDDO

    ! Bubble Sort
    !    CALL BubbleSort(dis(:),num(:),NS)
    ! Quick Sort
         CALL QsortC(dis(:),num(:))
    ! RESERVE SORT INFO
        DO j = 1,NS
            dist(i,j) = dis(j)
            numt(i,j) = num(j)
        ENDDO

     !   PRINT*,MINVAL(dis(:)),num(SUM(MINLOC(dis)))
     !   PRINT*,dist(i,1),numt(i,1)

    ENDDO
    PRINT*,"SORT OK!"
!************************************************************************************************************ 
    ! READ CLOUD DATA ACCORDING TO THE BCT 
	IF (stime%hr >= 0 .AND. stime%hr <= 7 ) THEN
	! BTC TO UTC
	  write(hr,'(i2.2)')  stime%hr+16
	  INQUIRE(FILE= TRIM(FILE_PATH_IN)//TRIM(LASTIME)//"/"//TRIM(LASTIME)//TRIM(HR)//".txt",EXIST = alive)
	  IF (alive) THEN
          OPEN(20,file= TRIM(FILE_PATH_IN)//TRIM(LASTIME)//"/"//TRIM(LASTIME)//TRIM(HR)//".txt")
          READ(20,*)   !DO NOT READ THE FIRST LINES
	  READ(20,*)
	  READ(20,*) (cld(i)%sta,cld(i)%lat,cld(i)%lon,cld(i)%slev,cld(i)%time,&
&cld(i)%vis,cld(i)%total_cld,cld(i)%low_cld,cld(i)%clev,i=1,NC)
          CLOSE(20)
          PRINT*,"READ CLOUD DATA OK!"
	  ELSE
	  WRITE(*,*) "ERROR!FILE NOT EXIST!"
	  ENDIF
	
	ELSE IF (stime%hr >= 8 .AND.stime%hr <= 23) THEN
	! BTC TO UTC
    	  write(hr,'(i2.2)')  stime%hr-8
	  INQUIRE(FILE= TRIM(FILE_PATH_IN)//TRIM(TIME)//"/"//TRIM(TIME)//TRIM(HR)//".txt",EXIST = alive)
	  IF (alive) THEN
          OPEN(20,file= TRIM(FILE_PATH_IN)//TRIM(TIME)//"/"//TRIM(TIME)//TRIM(HR)//".txt")
          READ(20,*)     !DO NOT READ THE FIRST LINES
	  READ(20,*)
	  READ(20,*) (cld(i)%sta,cld(i)%lat,cld(i)%lon,cld(i)%slev,cld(i)%time,&
&cld(i)%vis,cld(i)%total_cld,cld(i)%low_cld,cld(i)%clev,i=1,NC)
          CLOSE(20)
          PRINT*,"READ CLOUD DATA OK!"
	  ELSE
	  WRITE(*,*) "ERROR!FILE NOT EXIST!"
	  ENDIF

	ENDIF
!********************************************************************************************************
    ! REQUIRE FOR VIS&CLOUD DATA
	IF (stime%hr >= 8 .AND.stime%hr <= 23) THEN
        DO i = 1,NA
	    air(i)%time      = stime
            air(i)%vis       = 0
            air(i)%total_cld = 0
            air(i)%low_cld   = 0
            air(i)%clev      = 0
            air(i)%sta       = 0
            air(i)%dis       = 0.
           j = 1
        PD: DO WHILE( dist(i,j) <= R )
           ! PRINT*,dist(i,j),"ENTER WHILE"
            BL: DO k = 1,NC
            ! require from the nearest sta and the hr
                IF (cld(k)%sta == numt(i,j) )THEN
                    ! judge if the sta clddata is none,if not ,give it to air
                    IF (cld(k)%vis /= 999999 .OR. cld(k)%total_cld /= 999999 .OR. cld(k)%clev /= 999999&
&.OR.cld(k)%low_cld /= 999999 )THEN
                        air(i)%vis       =  cld(k)%vis
                        air(i)%total_cld = cld(k)%total_cld
                        air(i)%low_cld   = cld(k)%low_cld
                        air(i)%clev      =   cld(k)%clev
                        air(i)%sta       = numt(i,j)
                        air(i)%dis       = dist(i,j)
                        EXIT PD
			ELSE
			CYCLE BL
                    ENDIF
                ENDIF
            ENDDO BL
            j = j+1
        ENDDO PD
        ! if the air data is none ,give it the value
        IF ( air(i)%vis == 0.AND.air(i)%total_cld == 0.AND.air(i)%low_cld == 0.AND.air(i)%clev == 0 )THEN
            !PRINT*,"NO CLD INFO IN THE REFERENCE RANGE!"
            air(i)%vis       = 999999
            air(i)%total_cld = 999999
            air(i)%low_cld   = 999999
            air(i)%clev      = 999999
            air(i)%sta       = 99999
            air(i)%dis       = 99999.
        ENDIF
	ENDDO

	ELSE IF (stime%hr >= 0 .AND. stime%hr <= 7 ) THEN
        DO i = 1,NA
            air(i)%time     = stime
            air(i)%vis       = 0
            air(i)%total_cld = 0
            air(i)%low_cld   = 0
            air(i)%clev      = 0
            air(i)%sta       = 0
            air(i)%dis       = 0.
            j = 1
        PD1: DO WHILE( dist(i,j) <= R )
           ! PRINT*,dist(i,j),"ENTER WHILE"
            BL1: DO k = 1,NC
            ! require from the nearest sta and the h
                 IF (cld(k)%sta == numt(i,j) )THEN
                    ! judge if the sta clddata is none,if not ,give it to air
                    IF (cld(k)%vis /= 999999 .OR. cld(k)%total_cld /= 999999.OR. cld(k)%clev /= 999999&
&.OR.cld(k)%low_cld /= 999999 )THEN
                        air(i)%vis       = cld(k)%vis
                        air(i)%total_cld = cld(k)%total_cld
                        air(i)%low_cld   = cld(k)%low_cld
                        air(i)%clev      = cld(k)%clev
                        air(i)%sta       = numt(i,j)
                        air(i)%dis       = dist(i,j)
                        EXIT PD1
                    ELSE
                        j = j+1
                        CYCLE PD1
                    ENDIF

                ENDIF
            ENDDO BL1
            j = j+1
        ENDDO PD1
        ! if the air data is none ,give it the value
        IF ( air(i)%vis == 0.AND.air(i)%total_cld == 0.AND.air(i)%low_cld == 0.AND.air(i)%clev == 0 )THEN
            !PRINT*,"NO CLD INFO IN THE REFERENCE RANGE!"
            air(i)%vis       = 999999
            air(i)%total_cld = 999999
            air(i)%low_cld   = 999999
            air(i)%clev      = 999999
            air(i)%sta       = 99999
            air(i)%dis       = 99999.
        ENDIF

        ENDDO
    ENDIF
!**********************************************************************************************************
    ! OUTPUT INTO FILE
    WRITE(hr,'(i2.2)')  stime%hr
    OPEN(30,file= TRIM(FILE_PATH_OUT)//TRIM(TIME)//"/"//TRIM(FILE_NAME)//"_"//TRIM(TIME)//"_"//TRIM(hr)//".txt")
    WRITE(30,200) (air(i)%num,air(i)%lat,air(i)%lon,air(i)%time,air(i)%vis&
& ,air(i)%total_cld,air(i)%low_cld,air(i)%clev,air(i)%sta,air(i)%dis, i=1,NA)
    WRITE(*,"(1X,A,I4,A)") "OUTPUT BTC",stime%hr," OK!"
    CLOSE(30)

    PRINT*,"MISSION COMPLETED!"


100 FORMAT(A,1x,F5.2,2x,F6.2,2x,I5)
200 FORMAT(I3.3,2X,F5.2,2X,F6.2,2X,4I4,2x,I10,4X,I10,4X,I10,4X,I10,4X,I10,4X,f16.6)

    END
!************************************************************************************************************
! Bubble Sort
Subroutine BubbleSort(array,attach,NT)
    implicit none
    integer i,j,NT
    real array(NT)
    integer attach(NT)

    do i=1,NT-1
        do j=NT-1,2,-1
            if (array(j)<array(j-1)) then
                call swap(array,attach,j-1,j)
            endif
        enddo
    enddo
    END
! SWAP ARRAY
Subroutine swap(array,attach,i,j)
    implicit none
    integer i,j
    real array(1)
    real temp
    integer attach(1)
    integer cache

    temp      = array(i)
    array(i)  = array(j)
    array(j)  = temp

    cache     = attach(i)
    attach(i) = attach(j)
    attach(j) = cache

    END

Function getfilelines(file_name,file_path,file_id)
    implicit none
    integer :: row,file_id,statuss
    integer :: getfilelines
    character*150 :: file_name,file_path
    row = 0
    open(file_id,file = ""//trim(file_path)//""//trim(file_name)//".MIC")
    do while(.true.)
        read(file_id,*,iostat = statuss)
        row = row + 1
        if (statuss /= 0) exit
    end do
    close(file_id)
    rewind(file_id)

    getfilelines = row
    return

end
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function lastday(inyear,inmonth,inday)
    implicit none
    integer :: shuzu1(12)
    integer :: inyear,inmonth,inday
    integer :: inyear0,inmonth0,inday0
    integer :: inyear1,inmonth1,inday1
    character*8 :: cdate,cdate0,cdate1
    character*4 :: cyear,cyear0,cyear1
    character*2 :: cmonth,cmonth0,cmonth1
    character*2 :: cday,cday0,cday1
    character*8 :: lastday  
!***************************************************************
shuzu1=RESHAPE((/31,28,31,30,31,30,31,31,30,31,30,31/),(/12/))
!***************************************************************
       
  if(MOD(inyear,4)==0) then
  shuzu1(2)=29
  endif
  if ((inday.NE.1).AND.(inday.NE.shuzu1(inmonth))) then
	 inday0 = inday - 1
	 inmonth0 = inmonth
	 inyear0 = inyear
	 inday1 = inday + 1
	 inmonth1 = inmonth
	 inyear1 = inyear
  elseif (inday .eq. 1) then
	 inday1 = inday + 1
	 inmonth1 = inmonth
	 inyear1 = inyear
	 if (inmonth.ne.1) then
		inmonth0 = inmonth -1
		inyear0 = inyear
		inday0 = shuzu1(inmonth0)
	elseif (inmonth .eq.1) then
		inmonth0 = 12
		inyear0 = inyear -1
		inday0 = 31 
	endif
  elseif (inday.eq.shuzu1(inmonth)) then
	inday0 = inday-1
	inmonth0 = inmonth
	inyear0 = inyear
	if (inmonth.ne.12) then
		inmonth1 = inmonth + 1
		inyear1 = inyear
		inday1 = 1
	elseif (inmonth.eq.12) then
		inmonth1 = 1
		inyear1 = inyear + 1
		inday1 = 1
	endif
  endif

	write(cyear0,'(i4.4)') inyear0
	write(cmonth0,'(i2.2)') inmonth0
	write(cday0,'(i2.2)') inday0
	cdate0 = cyear0//cmonth0//cday0
	write(cyear1,'(i4.4)') inyear1
	write(cmonth1,'(i2.2)') inmonth1
	write(cday1,'(i2.2)') inday1
	cdate1 = cyear1//cmonth1//cday1
	
	lastday = cdate0
	
	return
	
	
	end 






