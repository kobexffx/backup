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
        INTEGER ::  vis,clev,wea
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
        REAL :: vis,clev,total_cld,low_cld
        REAL :: a,b,c,d
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
!*********************************************************
Program HX003
    USE TYPEDEF
    USE QsortC_Module
    IMPLICIT NONE
    INTEGER,EXTERNAL :: getfilelines
    EXTERNAL BubbleSort,swap
    CHARACTER*8,EXTERNAL :: lastday
    INTEGER,PARAMETER::NA = 354,NS = 2421
    REAL,PARAMETER :: R = 1.0
!****************************************************
    INTEGER :: i,j,N,k,it,ia,ib
    INTEGER :: statuss,filelines,fileid
    INTEGER,ALLOCATABLE :: sta_num(:),wea(:)
    CHARACTER*150,ALLOCATABLE :: arec(:)
!****************************************************
    TYPE(airport_info) :: air(NA)
    TYPE(station_info) :: sta(NS)
    TYPE(time_info)    :: stime
!****************************************************
    INTEGER :: date_time(8)
    REAL :: dis(NS),dist(NA,NS)
    INTEGER :: num(NS),numt(NA,NS)
    CHARACTER*2   :: HR
    CHARACTER*8   :: TIME,LASTIME
    CHARACTER*100 :: b(3),FCT(6),NAM(4),INTRO(2)
    CHARACTER*150 :: FILE_NAME,PATH,FILE_PATH_IN,FILE_PATH_OUT
    CHARACTER*150 :: INPUT_NAME,INPUT_PATH
!******************************************************************
    !DEFINE FORCAST TIME
	DATA FCT/'000','012','018','024','048','072'/
	DATA NAM/'ECRT06','EMX','ETAMIX1','ETAMIX2'/
	DATA INTRO/'12','24'/
!******************************************************************
	! READ DATE INFO
    OPEN(50,file="filedate.txt")
    READ(50,'(BN,a50)') TIME
    PRINT*,"READ TIME:", TRIM(TIME)," OK!"
    CLOSE(50)
    read(time(1:4),'(i4.4)') stime%year
    read(time(5:6),'(i2.2)') stime%mon
    read(time(7:8),'(i2.2)') stime%day

    ! READ SYSTEM TIME
    !CALL date_and_time(b(1),b(2),b(3),date_time)
    !stime%year = date_time(1)
    !stime%mon  = date_time(2)
    !stime%day  = date_time(3)
    stime%hr  = 20!date_time(5)
    WRITE(HR,"(I2.2)") stime%hr
    write(*,"(1x,a,a)"),"TEST TIME:",TRIM(TIME)//HR

    LASTIME = lastday(stime%year,stime%mon,stime%day)

    ! READ PATH INFO
    OPEN(40,file="path.txt")
    READ(40,'(BN,a50)') PATH
    PRINT*,"READ PATH:", TRIM(PATH)," OK!"
    CLOSE(40)

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
!*************************************************************************
    ! SORT FOR THE STATION NEAR EVERY AIRPORT
    DO i = 1,NA
        DO j = 1, NS
            dis(j) = SQRT((sta(j)%lat-air(i)%lat)**2+(sta(j)%lon-air(i)%lon)**2)
            num(j) = sta(j)%num
        ENDDO

    ! Bubble Sort
    !   CALL BubbleSort(dis(:),num(:),NS)
    ! QUICK SORT
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
!***********************************************************************
    ! DEFINE IN&OUT PATH
    FILE_PATH_IN  =  TRIM(PATH)//'/input/cloud/'//TRIM(TIME)
    FILE_PATH_OUT = TRIM(PATH)//'/output/surface/forecast/'//TRIM(TIME)

    ! DEFINE FIEL_NAME FIXED PART
    FILE_NAME   = "HX_F_DISASTROUS_WEATHER"
!***********************************************************************
IF ( stime%hr >= 8 .AND. stime%hr <= 23) THEN
WRITE(HR,"(I2.2)") stime%hr-8
FC: DO ib = 1,2
       DO it = 1,2
          DO ia  = 1,4
        INPUT_NAME = "SEVP_NMC_WEFC_WTFC_CHN_MUT-"//TRIM(NAM(ia))//"-"//TRIM(TIME)//"-"//TRIM(HR)&
&//"00-"//TRIM(FCT(it))//"-"//TRIM(INTRO(ib))
        !write(*,"(1x,a)")  INPUT_NAME//".MIC"
!***************getFILELINES***********************************************
    fileID    = 10
    filelines = getfilelines(INPUT_NAME,FILE_PATH_IN,fileID)
    if (filelines == -1) cycle 
!************************************************************************
    allocate(arec(filelines))
!***************************************************************************************
    open(10,file = TRIM(FILE_PATH_IN)//"/"//TRIM(INPUT_NAME)//".MIC")
    read(10,'(a)') (arec(i),i=1,filelines)
    close(10)
!********************************************************************************************
    do i =1,filelines
        if (arec(i) == "STATION_SITUATION") then
            N = filelines - i
            exit
        end if
    end do
    !PRINT*,filelines,N
!*********************************************************************************************
    allocate(sta_num(N+1))
    allocate(wea(N+1))
!*********************************************************************************************
    do i = 1,N
         j = 1
    if (arec(i)(1:1) == "5".OR.arec(i)(1:1) == "4") then
        read(arec(i),"(i5,1x,i3)") sta_num(j),wea(j)
       ! write(*,"(a,2x,i5,2x,i3)") arec(i),sta_num(j),wea(j)
    end if
        j = j+1
    enddo
!**********************************************************************************************
    deallocate(arec)
!**********************************************************************************************
    ! REQUIRE FOR TARGET FORCAST DATA
    DO  i = 1,NA
        air(i)%time = stime
	air(i)%wea = 999999
	air(i)%sta = 99999
	air(i)%dis = 99999.
        j = 1
        PD: DO WHILE( dist(i,j) <= R )
           ! PRINT*,dist(i,j),"ENTER WHILE"
            BL: DO k = 1,N
            ! require from the nearest sta and the hr
                IF ( sta_num(k) == numt(i,j)  ) THEN
                    ! judge if the sta clddata is none,if not ,give it to air
		        air(i)%wea   = wea(k)
                        air(i)%sta   = numt(i,j)
                        air(i)%dis   = dist(i,j)
                        EXIT PD
                ELSE
                        j = j+1
                        CYCLE PD

                ENDIF
            ENDDO BL

        ENDDO PD


    ENDDO
!**************************************************************************
	deallocate(sta_num)
        deallocate(wea)
!***************************************************************************
    ! OUTPUT INTO FILE
	WRITE(HR,"(I2.2)") stime%hr
    OPEN(30,file= TRIM(FILE_PATH_OUT)//"/"//TRIM(FILE_NAME)//"_"//TRIM(TIME)//TRIM(HR)//".txt",ACCESS="APPEND")
    WRITE(30,"(1x,4I4,2X,a)")  stime, FCT(it)//INTRO(ib)
    WRITE(30,200) (air(i)%num,air(i)%lat,air(i)%lon,air(i)%time,air(i)%wea,air(i)%sta,air(i)%dis, i=1,NA)
    CLOSE(30)



	ENDDO
  ENDDO
ENDDO FC
!**********************************************************************************************
ELSE IF ( stime%hr <= 7 .AND. stime%hr >= 0) THEN
WRITE(HR,"(I2.2)") stime%hr+16
     DO ib = 1,2
       DO it = 1,2
	    DO ia  = 1,4
        INPUT_NAME = "SEVP_NMC_WEFC_WTFC_CHN_MUT-"//TRIM(NAM(ia))//"-"//TRIM(LASTIME)//"-"//TRIM(HR)&
&//"00-"//TRIM(FCT(it))//"-"//TRIM(INTRO(ib))
        !write(*,"(1x,a)")  INPUT_NAME//".MIC"
!***************getFILELINES***********************************************
    fileID    = 10
    filelines = getfilelines(INPUT_NAME,FILE_PATH_IN,fileID)
    if (filelines == -1) cycle 
!************************************************************************
    allocate(arec(filelines))
!***************************************************************************************
    open(10,file = TRIM(FILE_PATH_IN)//"/"//TRIM(INPUT_NAME)//".MIC")
    read(10,'(a)') (arec(i),i=1,filelines)
    close(10)
!********************************************************************************************
    do i =1,filelines
        if (arec(i) == "STATION_SITUATION") then
            N = filelines - i
            exit
        end if
    end do
    !PRINT*,filelines,N
!*********************************************************************************************
    allocate(sta_num(N+1))
    allocate(wea(N+1))
!*********************************************************************************************
    do i = 1,N
         j = 1
    if (arec(i)(1:1) == "5".OR.arec(i)(1:1) == "4") then
        read(arec(i),"(i5,1x,i3)") sta_num(j),wea(j)
       ! write(*,"(a,2x,i5,2x,i3)") arec(i),sta_num(j),wea(j)
    end if
        j = j+1
    enddo
!**********************************************************************************************
    deallocate(arec)
!**********************************************************************************************
    ! REQUIRE FOR TARGET FORCAST DATA
    DO  i = 1,NA
        air(i)%time = stime
	air(i)%wea = 999999
	air(i)%sta = 99999
	air(i)%dis = 99999.
        j = 1
        PD1: DO WHILE( dist(i,j) <= R )
           ! PRINT*,dist(i,j),"ENTER WHILE"
            BL1: DO k = 1,N
            ! require from the nearest sta and the hr
                IF ( sta_num(k) == numt(i,j)  ) THEN
                    ! judge if the sta clddata is none,if not ,give it to air
			            air(i)%wea   = wea(k)
                        air(i)%sta   = numt(i,j)
                        air(i)%dis   = dist(i,j)
                        EXIT PD1
                ELSE
                        j = j+1
                        CYCLE PD1

                ENDIF
            ENDDO BL1

        ENDDO PD1


    ENDDO
!**************************************************************************
	deallocate(sta_num)
        deallocate(wea)
!***************************************************************************
    ! OUTPUT INTO FILE
	WRITE(HR,"(I2.2)") stime%hr
    OPEN(30,file= TRIM(FILE_PATH_OUT)//"/"//TRIM(FILE_NAME)//"_"//TRIM(TIME)//TRIM(HR)//".txt",ACCESS="APPEND")
    WRITE(30,"(1x,4I4,2X,a)")  stime, FCT(it)//INTRO(ib)
    WRITE(30,200) (air(i)%num,air(i)%lat,air(i)%lon,air(i)%time,air(i)%wea,air(i)%sta,air(i)%dis, i=1,NA)
    CLOSE(30)



	ENDDO
  ENDDO
ENDDO 
ENDIF

WRITE(*,"(1X,A,4I4,A)") "OUTPUT BTC:",stime," OK!"
PRINT*,"MISSION COMPLETED!"

100 FORMAT(A,1x,F5.2,2x,F6.2,2x,I5)
200 FORMAT(I4.3,2X,F8.2,2X,F8.2,2X,4I4,5X,I8,5X,I8,5X,f12.6)

End Program

function getfilelines(file_name,file_path,file_id)
    implicit none
    integer :: row,file_id,statuss
    integer :: getfilelines
    character*150 :: file_name,file_path
	logical  :: alive
    row = 0

	! JUDGE WETHER THE TARGET DATA EXIST
	inquire(file =  trim(file_path)//"/"//trim(file_name)//".MIC",EXIST = alive)

	if (alive) then
        open(file_id,file = trim(file_path)//"/"//trim(file_name)//".MIC")
        do while(.true.)
              read(file_id,*,iostat = statuss)
              row = row + 1
              if (statuss /= 0) exit
        end do
        close(file_id)
        rewind(file_id)
    else
	    write(*,'(1x,A)') TRIM(FILE_NAME)//".MIC IS NOT EXIST!"
	endif

	getfilelines = row-1

    return

end
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
	
Function lastday(inyear,inmonth,inday)
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
